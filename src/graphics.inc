.ifndef _GRAPHICS_INC_
  _GRAPHICS_INC_ = 1

  .include "graphics-defs.inc"

  .segment "ZEROPAGE"

  .import palette_fade
  .import metasprite_x
  .import metasprite_y
  .import subscreen_width
  .import subscreen_height
  .import subscreen_source_addr
  .import ppu_addr

  .segment "CODE"

  .import load_palettes
  .import draw_subscreen
  .import clear_subscreen
  .import draw_text
  .import draw_metasprite
  .import draw_sprite
  .import reset_sprite_counter
  .import erase_remaining_sprites
  .import refresh_oam
  .import set_scroll
.endif
