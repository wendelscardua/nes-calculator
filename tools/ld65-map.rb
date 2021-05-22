#!/usr/bin/ruby
# frozen_string_literal: true

require 'json'

def hex(hex_value)
  hex_value.to_i(16)
end

def scale(x, total)
  x.to_f / total
end

if ARGV.empty?
  puts 'Arguments: input-file prg-banks chr-banks json-output'
  exit 1
end

input = ARGV[0]
prg_banks = ARGV[1].to_i
chr_banks = ARGV[2].to_i
output = ARGV[3]

map_text = File.readlines(input, chomp: true)

module_data = []

mode = nil

map_text.each do |line|
  case line
  when 'Modules list:'
    mode = :modules_list
  when ''
    mode = nil
  end

  case mode
  when :modules_list
    if (m = line.match(/\A\s*(?<name>[^.]*\.o):/))
      module_data << { module: m['name'], segments: [] }
    elsif (m = line.match(/\A\s+(?<segment>\S+)\s+Offs=(?<offset>\S+)\s+Size=(?<size>\S+)\s+Align=(?<align>\S+)\s+Fill=(?<fill>\S+)/))
      module_data.last[:segments] << {
        segment: m['segment'],
        offset: hex(m['offset']),
        size: hex(m['size']),
        align: hex(m['align']),
        fill: hex(m['fill'])
      }
    end
  end
end

categories = {
  'HEADER' => { size: 10, segments: ['HEADER'] },
  'PRG' => { size: 0x4000 * prg_banks, segments: %w[CODE RODATA VECTORS] },
  'CHR' => { size: 0x2000 * chr_banks, segments: ['CHR'] },
  'ZEROPAGE' => { size: 0x100, segments: ['ZEROPAGE'] },
  'STACK' => { size: 0x100, segments: ['STACK'] },
  'RAM' => { size: 0x800 - 0x200, segments: %w[OAM FAMITONE BSS] }
}

json_map = { name: 'ld65-map', children: [] }

puts 'Usage per category:'
categories.each do |cat_name, cat_data|
  puts "#{cat_name}:"
  level_1_child = { name: cat_name, children: [] }
  json_map[:children] << level_1_child

  free = cat_data[:size]
  cat_data[:segments].each do |seg_name|
    usage = module_data.flat_map { |mod| mod[:segments] }
                       .select { |segment| segment[:segment] == seg_name }
                       .map { |segment| segment[:size] }
                       .sum
    puts "  #{seg_name}: #{usage}"
    level_2_child = { name: seg_name, children: [] }
    level_1_child[:children] << level_2_child

    module_data.map do |mod|
      [
        mod[:module],
        mod[:segments].find { |seg| seg[:segment] == seg_name }
                      .then { |seg| seg ? seg[:size] : nil }
      ]
    end.reject { |_m, s| s.nil? }
               .sort_by { |_m, s| s }
               .reverse
               .each do |m, s|
      puts "    #{m}: #{s} bytes"
      level_3_child = { name: m, size: s, value: scale(s, cat_data[:size].to_f), group: cat_name, subgroup: seg_name }
      level_2_child[:children] << level_3_child
    end

    free -= usage
  end
  level_1_child = { name: 'Free', size: free, value: scale(free, cat_data[:size].to_f), group: cat_name }
  json_map[:children] << level_1_child
  puts "  Free: #{free}"
end

File.open(output, 'w') do |file|
  file.write json_map.to_json
end
