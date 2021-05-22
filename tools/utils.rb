#!/usr/bin/ruby
# frozen_string_literal: true

# Util methods for ca65 code generationa
module Utils
  def ca65escape(string, charmap: {})
    string = string.chars.map do |c|
      if charmap[c]
        format('", $%02x, "', charmap[c])
      else
        c
      end
    end.join.gsub(/ "",/, '')
    %("#{string}", $ff)
  end

  def bytes(array)
    array.map { |item| byte(item) }.join(', ')
  end

  def byte(value)
    if value.is_a?(String)
      value
    else
      format('$%02x', value.negative? ? 0x100 + value : value)
    end
  end

  def word(value)
    format('$%04x', value.negative? ? 0x10000 + value : value)
  end
end
