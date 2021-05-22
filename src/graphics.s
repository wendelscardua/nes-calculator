.include "graphics-defs.inc"
.include "nmi.inc"
.include "temps.inc"
.include "unrle.inc"
.include "vram-buffer.inc"
.include "../assets/metasprites.inc"

.segment "ZEROPAGE"

.export palette_fade
.export metasprite_x
.export metasprite_y
.export subscreen_width
.export subscreen_height
.export subscreen_source_addr
.export ppu_addr

palette_fade: .res 1
metasprite_x: .res 1
metasprite_y: .res 1
subscreen_width: .res 1
subscreen_height: .res 1
ppu_addr: .res 2
subscreen_source_addr: .res 2
sprite_counter: .res 1

.segment "OAM"

.struct Sprite
  ycoord .byte
  tile .byte
  flag .byte
  xcoord .byte
.endstruct

oam_sprites:
.repeat 64
  .tag Sprite
.endrepeat
.zeropage

.segment "CODE"

; cobbles Y
.macro LOAD_PALETTES pal_addr
  .local loop
  .local no_negative

  PHA
  TXA
  PHA

  vb_alloc (2 + $20 + 2)

  vb_ppu_addr $3f00

  LDY #$00
loop:
  LDA pal_addr,Y
  SEC
  SBC palette_fade
  CMP #$3e
  BCC no_overflow
  LDA palette_fade
  BMI to_white
  LDA #$0f
  JMP no_overflow
to_white:
  LDA #$30
no_overflow:
  STA vram_buffer, X
  INX
  INY
  CPY #$20
  BNE loop

  vb_push #$ff
  vb_close

  PLA
  TAX
  PLA
.endmacro

.export load_palettes
.proc load_palettes
  ; cobbles Y
  LOAD_PALETTES palettes
  RTS
.endproc

.export draw_subscreen
.proc draw_subscreen
  ; input: subscreen_width, subscreen_height = size of rectangle
  ;        ppu_addr = start point on ppu
  ;        subscreen_source_addr = start point of data
  ; cobbles X, Y
row_loop:
  DEC subscreen_height
  BMI end_row_loop

  vb_alloc (20 + 2 + 2)

  LDA ppu_addr+1
  EOR #$80
  vb_pusha
  vb_push ppu_addr

  LDY #0
column_loop:
  vb_push {(subscreen_source_addr), Y}
  INY
  CPY subscreen_width
  BNE column_loop
  vb_push #$ff
  vb_close

  LDA subscreen_width
  CLC
  ADC subscreen_source_addr
  STA subscreen_source_addr
  BCC :+
  INC subscreen_source_addr+1
:

  LDA #$20
  CLC
  ADC ppu_addr
  STA ppu_addr
  BCC :+
  INC ppu_addr+1
:
  JMP row_loop
end_row_loop:
  LDX #1
  JSR wait_frames
  RTS
.endproc

.export draw_text
.proc draw_text
  ; input: ppu_addr = start point on ppu
  ;        subscreen_source_addr = start point of data
  ; cobbles X, Y
  ; similar to draw_subscreen but using linebreaks ($0a)
  ; ... and $ff as end of string
row_loop:
  vb_alloc (21 + 2 + 2)

  vb_push ppu_addr+1
  vb_push ppu_addr

  LDY #0
column_loop:
  LDA (subscreen_source_addr), Y
  INY
  CMP #$0a
  BEQ end_column_loop
  CMP #$ff
  BNE :+
  vb_push #$00
  vb_close
  JMP end_row_loop
:
  vb_pusha
  JMP column_loop
end_column_loop:
  vb_push #$00
  vb_close

  LDA #$20
  CLC
  ADC ppu_addr
  STA ppu_addr
  BCC :+
  INC ppu_addr+1
:

  TYA
  CLC
  ADC subscreen_source_addr
  STA subscreen_source_addr
  BCC :+
  INC subscreen_source_addr+1
:
  JMP row_loop
end_row_loop:
  LDX #1
  JSR wait_frames
  RTS
.endproc

.export clear_subscreen
.proc clear_subscreen
  ; input: subscreen_width, subscreen_height = size of rectangle
  ;        ppu_addr = start point on ppu
  ; cobbles X, Y
row_loop:
  DEC subscreen_height
  BMI end_row_loop

  vb_alloc (20 + 2 + 2)

  vb_push ppu_addr+1
  vb_push ppu_addr

  LDY #0
column_loop:
  vb_push #$4b ; XXX - alternative space
  INY
  CPY subscreen_width
  BNE column_loop
  vb_push #$00
  vb_close

  LDA #$20
  CLC
  ADC ppu_addr
  STA ppu_addr
  BCC :+
  INC ppu_addr+1
:
  JMP row_loop
end_row_loop:
  LDX #1
  JSR wait_frames
  RTS
.endproc

.export reset_sprite_counter
.proc reset_sprite_counter
  ; resets all sprite counters
  LDA #$00
  STA sprite_counter
  RTS
.endproc

.export draw_metasprite
.proc draw_metasprite
  ; input: X = metasprite index
  ;        metasprite_x, metasprite_y = origin coordinates
  ; cobbles X, Y
  LDA metasprites_l, X
  STA subscreen_source_addr
  LDA metasprites_h, X
  STA subscreen_source_addr + 1

  LDY #$0
  LDX sprite_counter
loop:
  CPX #MAX_SPRITES
  BCS exit_loop
  LDA (subscreen_source_addr), Y ; delta x
  CMP #128
  BEQ exit_loop
  INY
  CLC
  ADC metasprite_x

  ;  trying to skip offscreen tiles
  CMP #$f8
  BCC noskip
  INY
  INY
  INY
  JMP loop
noskip:


  STA oam_sprites+Sprite::xcoord, X

  LDA (subscreen_source_addr), Y ; delta y
  INY
  SEC
  SBC #$01 ; align sprite with background
  CLC
  ADC metasprite_y
  STA oam_sprites+Sprite::ycoord, X

  LDA (subscreen_source_addr), Y ; tile
  INY
  STA oam_sprites+Sprite::tile, X
  LDA (subscreen_source_addr), Y ; flags
  INY
  STA oam_sprites+Sprite::flag, X
  .repeat .sizeof(Sprite)
    INX
  .endrepeat
  JMP loop
exit_loop:
  STX sprite_counter
  RTS
.endproc

.export draw_sprite
.proc draw_sprite
  ; draws a single sprite centered at x, y
  ; input: X = sprite tile, Y = sprite flags
  ;        metasprite_x, metasprite_y = center coordinates
  STX temp_a

  LDX sprite_counter

  CPX #MAX_SPRITES
  BCC :+
  RTS
:

  LDA metasprite_x
  SEC
  SBC #$04

  ;  trying to skip offscreen tiles
  CMP #$f8
  BCC noskip
  RTS
noskip:

  STA oam_sprites+Sprite::xcoord, X

  LDA metasprite_y
  SEC
  SBC #($04 + $01) ; align sprite with background
  STA oam_sprites+Sprite::ycoord, X

  LDA temp_a
  STA oam_sprites+Sprite::tile, X
  TYA
  STA oam_sprites+Sprite::flag, X
  .repeat .sizeof(Sprite)
    INX
  .endrepeat
  STX sprite_counter
  RTS
.endproc

.export erase_remaining_sprites
.proc erase_remaining_sprites
  ; cobbles X
  LDX sprite_counter
  LDA #$F0
: STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
    INX
  .endrepeat
  BNE :-
  RTS
.endproc

.export refresh_oam
.proc refresh_oam
  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  RTS
.endproc

.export set_scroll
.proc set_scroll
  LDA PPUSTATUS

  LDA #$00
  STA PPUSCROLL ; horizontal scroll
  STA PPUSCROLL

  LDA #SCREEN_ON_FLAGS
  STA PPUCTRL
  RTS
.endproc

.segment "RODATA"

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

.segment "CHR"

.incbin "../assets/chr/bg.chr"
.incbin "../assets/chr/sprite.chr"
