#!/usr/bin/ruby
# frozen_string_literal: true

# Converts tiled data into Bare Metal level data
#
# Usage:
# ruby tiled.rb level level.tmx level.s
# or
# ruby tiled.rb metatiles metatiles.tsx metatiles.s

require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'

  gem 'nokogiri'
  gem 'pry'
end

require_relative './utils'

# Representation of Tiled metatiles
# For Bare Metal we assume metatiles are 2x2
class Metatiles
  attr_accessor :metatiles, :mapping

  def initialize(file)
    @file = file
  end

  def parse
    @metatiles = []
    @mapping = {}

    tile_document = Nokogiri::XML(File.read(@file))
    image_name = tile_document.xpath('//tileset/image').first['source']
    image_document = Nokogiri::XML(File.read(File.join(File.dirname(@file), image_name)))

    tiles_array = image_document.xpath('//data')
                                .first
                                .text
                                .scan(/\d+/)
                                .map(&:to_i)
                                .map { |tile| tile.zero? ? nil : tile - 1 }

    tiles_array.each.with_index do |tile, i|
      next if tile.nil?

      row = i / 32
      col = i % 32
      metatile_source_index = (row / 2) * 16 + (col / 2)
      metatile_inner_index = (row % 2) * 2 + (col % 2)

      @mapping[metatile_source_index] ||= @metatiles.size
      metatile_index = @mapping[metatile_source_index]
      @metatiles[metatile_index] ||= {
        id: metatile_source_index,
        index: metatile_index,
        tiles: [nil, nil, nil, nil],
        solid: false,
        goal: false,
        poison: false,
        boing: false,
        palette: 0
      }
      @metatiles[metatile_index][:tiles][metatile_inner_index] = tile
    end

    @metatiles.each do |metatile|
      i = metatile[:id]
      next if tile_document.xpath("//tile[@id=#{i}]").empty?

      %i[solid goal poison boing].each do |prop|
        metatile[prop] =
          tile_document.xpath("//tile[@id=#{i}]/properties/property[@name='#{prop}']").first['value'] == 'true'
      end
      metatile[:palette] =
        tile_document.xpath("//tile[@id=#{i}]/properties/property[@name='palette']").first['value'].to_i
    end
  end

  def write(file)
    File.open(file, 'wb') do |f|
      f.puts '.segment "RODATA"'
      f.puts '.export metatile_ul, metatile_ur, metatile_dl, metatile_dr, metatile_pal'
      f.puts "metatile_ul: .byte #{@metatiles.map { |mt| byte(mt[:tiles][0]) }.join(', ')}"
      f.puts "metatile_ur: .byte #{@metatiles.map { |mt| byte(mt[:tiles][1]) }.join(', ')}"
      f.puts "metatile_dl: .byte #{@metatiles.map { |mt| byte(mt[:tiles][2]) }.join(', ')}"
      f.puts "metatile_dr: .byte #{@metatiles.map { |mt| byte(mt[:tiles][3]) }.join(', ')}"
      f.puts "metatile_pal: .byte #{@metatiles.map { |mt| byte(mt[:palette]) }.join(', ')}"
    end
  end

  include Utils
end

# Representation of Tiled level
class Level
  def initialize(file)
    @file = file
    @turtle_x = 0
    @turtle_y = 0
    @turtle_direction = 0
    @strawberry = []
    @command_queue = []
    @command_ppu_addr = 0
  end

  def parse
    level_document = Nokogiri::XML(File.read(@file))
    metatiles_filename = level_document.xpath('//tileset').first['source']
    metatiles_document = Metatiles.new(File.join(File.dirname(@file), metatiles_filename))
    metatiles_document.parse
    metatiles = metatiles_document.metatiles
    mapping = metatiles_document.mapping

    # turtle
    turtle =
      level_document.xpath('//objectgroup[@name="coordinates"]' \
                           '/object[@name="turtle"]')
                    .first
    @turtle_x = snap_coordinate(turtle['x'].to_f.round.to_i)
    @turtle_y = snap_coordinate(turtle['y'].to_f.round.to_i)
    @turtle_direction = direction(turtle['type'])

    # strawberries
    @strawberry = []
    level_document.xpath('//objectgroup[@name="coordinates"]/object[@name="strawberry"]')
                  .each do |object|
      @strawberry << {
        x: snap_coordinate(object['x'].to_f.round.to_i),
        y: snap_coordinate(object['y'].to_f.round.to_i)
      }
    end

    # commands
    commands =
      level_document.xpath('//objectgroup[@name="coordinates"]' \
                           '/object[@name="commands"]')
                    .first
    commands_x = snap_coordinate(commands['x'].to_f.round.to_i) / 16
    commands_y = snap_coordinate(commands['y'].to_f.round.to_i) / 16

    @command_ppu_addr = 0x2000 + commands_y * 0x40 + commands_x * 0x2

    @command_queue = (commands['type'] || '').chars.map { |char| direction(char) }

    level_metatiles =
      level_document.xpath('//layer[@name="Map"]/data')
                    .text
                    .scan(/\d+/)
                    .map { |t| t.to_i - 1 }

    @bg_matrix = level_metatiles.map { |mt| metatiles[mapping[mt] || 0][:solid] ? 1 : 0 }
                                .each_slice(8)
                                .map { |slice| slice.reduce(0) { |acc, elem| 2 * acc + elem } }
    @poison_matrix = level_metatiles.map { |mt| metatiles[mapping[mt] || 0][:poison] ? 1 : 0 }
                                    .each_slice(8)
                                    .map { |slice| slice.reduce(0) { |acc, elem| 2 * acc + elem } }
    @boing_matrix = level_metatiles.map { |mt| metatiles[mapping[mt] || 0][:boing] ? 1 : 0 }
                                   .each_slice(8)
                                   .map { |slice| slice.reduce(0) { |acc, elem| 2 * acc + elem } }
    @goal_matrix = level_metatiles.map { |mt| metatiles[mapping[mt] || 0][:goal] ? 1 : 0 }
                                  .each_slice(8)
                                  .map { |slice| slice.reduce(0) { |acc, elem| 2 * acc + elem } }

    nametable_metatiles = level_metatiles.map { |mt| metatiles[mapping[mt]] }

    @nametable = rle(nametable_metatiles.map { |mt| mt[:index] })
    @attributes = make_attributes(nametable_metatiles)
  end

  def write(file)
    data_label = File.basename(@file, '.tmx').tr('-', '_')
    File.open(file, 'w') do |f|
      f.puts "#{data_label}_data:"
      f.puts ".word #{data_label}_nametable"
      f.puts ".byte #{byte(@turtle_x)}, #{byte(@turtle_y)}, #{byte(@turtle_direction)} ; turtle x y dir"
      @strawberry.each.with_index do |obj, index|
        f.puts "; strawberry ##{index}"
        f.puts ".byte #{byte(obj[:x])}, #{byte(obj[:y])}"
      end
      f.puts ".word #{word(@command_ppu_addr)} ; command queue ppu addr"
      @command_queue << 0xff
      f.puts ".byte #{@command_queue.map { |b| byte(b) }.join(', ')} ; command queue"
      f.puts ".word #{data_label}_bg_matrix"
      f.puts ".word #{data_label}_poison_matrix"
      f.puts ".word #{data_label}_boing_matrix"
      f.puts ".word #{data_label}_goal_matrix"
      f.puts "#{data_label}_bg_matrix:"
      f.puts ".byte #{@bg_matrix.map { |b| byte(b) }.join(', ')}"
      f.puts "#{data_label}_poison_matrix:"
      f.puts ".byte #{@poison_matrix.map { |b| byte(b) }.join(', ')}"
      f.puts "#{data_label}_boing_matrix:"
      f.puts ".byte #{@boing_matrix.map { |b| byte(b) }.join(', ')}"
      f.puts "#{data_label}_goal_matrix:"
      f.puts ".byte #{@goal_matrix.map { |b| byte(b) }.join(', ')}"
      f.puts "#{data_label}_nametable:"
      f.puts ".byte #{@nametable.map { |b| byte(b) }.join(', ')}"
      f.puts ".byte #{@attributes.map { |b| byte(b) }.join(', ')}"
    end
  end

  private

  include Utils

  def direction(char_direction)
    {
      'U' => 0,
      'D' => 1,
      'L' => 2,
      'R' => 3,
      'C' => 4,
      'G' => 5
    }.fetch(char_direction)
  end

  def make_attributes(metatiles)
    attributes = [0] * 64

    metatiles.each.with_index do |mt, i|
      row = i / 16
      col = i % 16
      attr_row = row / 2
      attr_col = col / 2
      attr_index = attr_row * 8 + attr_col
      attr_shift = 2 * ((row % 2) * 2 + col % 2)
      attributes[attr_index] |= mt[:palette] << attr_shift
    end

    attributes
  end

  def snap_coordinate(coordinate)
    coordinate / 16 * 16 + 8
  end

  def rle(bytes)
    rle_tag = 0
    rle_tag += 1 while bytes.include?(rle_tag)

    compressed_bytes = [rle_tag]
    last_byte = nil
    counter = 0
    bytes.each do |byte|
      if byte != last_byte || counter == 255 || (rle_tag == 255 && counter == 254)
        if counter.positive?
          compressed_bytes += [rle_tag, counter]
          last_byte = nil if counter == 255 || (rle_tag == 255 && counter == 254)
          counter = 0
        end
        compressed_bytes << byte
        last_byte = byte
      else
        counter += 1
      end
    end
    if counter.positive?
      compressed_bytes += [rle_tag, counter]
      counter = 0
    end
    compressed_bytes += [rle_tag, 0]
    compressed_bytes
  end
end

# Representation of Tiled conduits
class Conduits
  def initialize(path)
    @path = path
    @conduits = []
  end

  def parse
    Dir.children(@path)
       .select { |f| f =~ /level.*\.tmx/ }
       .each do |filename|
      level_document = Nokogiri::XML(File.read(File.join(@path, filename)))

      level_document.xpath('//layer[@name="Conduit"]')
                    .each do |conduit_layer|
        conduit_id = conduit_layer.xpath("properties/property[@name='conduit']").first['value'].to_i
        conduit_target = conduit_layer.xpath("properties/property[@name='target']").first.then do |prop|
          prop.nil? ? 'door' : prop['value']
        end
        conduit_arg = conduit_layer.xpath("properties/property[@name='arg']").first.then do |prop|
          prop.nil? ? 0 : prop['value'].to_i
        end

        @conduits[conduit_id] ||= ["conduit_targets::#{conduit_target}", conduit_arg]

        cond_data = {}

        conduit_layer.xpath('data').each do |conduit_data|
          conduit_data.text
                      .scan(/\d+/)
                      .map { |t| t.to_i - 1 }
                      .each
                      .with_index do |t, i|
            next unless t >= 0

            delta_attr = if i % 32 < 16
                           0
                         else
                           64
                         end

            row = i / 32
            col = i % 16
            attr_row = row / 2
            attr_col = col / 2
            attr_index = attr_row * 8 + attr_col
            attr_shift = 2 * ((row % 2) * 2 + col % 2)
            cond_data[attr_index + delta_attr] ||= 0
            cond_data[attr_index + delta_attr] |= (0b11 << attr_shift)
          end
        end

        @conduits[conduit_id] += cond_data.flatten
      end
    end
  end

  def write(file)
    File.open(file, 'wb') do |f|
      conduit_ptrs = @conduits.size.times.map { |i| "conduit_#{i}_data" }.join(', ')
      f.puts '.include "conduits-defs.inc"'
      f.puts '.segment "RODATA"'
      f.puts '.export conduit_ptrs_l, conduit_ptrs_h'
      f.puts ".define conduit_ptrs #{conduit_ptrs}"
      f.puts 'conduit_ptrs_l: .lobytes conduit_ptrs'
      f.puts 'conduit_ptrs_h: .hibytes conduit_ptrs'
      f.puts '; data format: target, arg, (attr index, attr mask)+, -1'
      @conduits.each.with_index do |conduit, index|
        f.puts "conduit_#{index}_data: .byte #{conduit.map { |b| byte(b) }.join(', ')}, $ff"
      end
    end
  end

  include Utils
end

mode, tiled_file, output = ARGV

document = case mode
           when 'level' then Level.new(tiled_file)
           when 'metatiles' then Metatiles.new(tiled_file)
           when 'conduits' then Conduits.new(tiled_file)
           end

document.parse
document.write(output)
