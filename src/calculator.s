.include "graphics.inc"
.include "math.inc"
.include "nmi.inc"
.include "readjoy.inc"
.include "temps.inc"
.include "unrle.inc"
.include "vram-buffer.inc"

.segment "ZEROPAGE"

.enum metatiles
  zero
  one
  two
  three
  four
  five
  six
  seven
  eight
  nine
  negative
  decimal
  empty
.endenum

accumulator: .res 8
input: .res 8
memory: .res 8
operator: .res 1
mantissa_digit: .res 1
mantissa_nibble: .res 1
decimal_point_active: .res 1

cursor_row: .res 1
cursor_column: .res 1

.segment "CODE"

.export calculator_init
.proc calculator_init
  SCREEN_OFF

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable
  STA rle_ptr
  LDA #>nametable
  STA rle_ptr+1
  JSR unrle

  VBLANK

  SCREEN_ON

  LDA #$30
  STA palette_fade
  JSR load_palettes
  LDX #FADE_DELAY
  JSR wait_frames

  LDA #$20
  STA palette_fade
  JSR load_palettes
  LDX #FADE_DELAY
  JSR wait_frames

  LDA #$10
  STA palette_fade
  JSR load_palettes
  LDX #FADE_DELAY
  JSR wait_frames

  LDA #$00
  STA palette_fade
  JSR load_palettes
  LDX #FADE_DELAY
  JSR wait_frames

  JSR clear_accumulator
  JSR clear_input

  ;  testing
  ;  lda #<pi
  ;  ldy #>pi
  ;  ldx #<w1
  ;  jsr copy2w
  ;
  ;  lda #<exp0
  ;  ldy #>exp0
  ;  ldx #<w2
  ;  jsr copy2w
  ;
  ;  ldx #operations::acot
  ;  jsr calc

  LDA #4
  STA cursor_row
  LDA #3
  STA cursor_column

  RTS
.endproc

.export calculator_io
.proc calculator_io
  LDA pressed_buttons
  AND #BUTTON_UP
  BEQ :+
  LDA cursor_row
  BEQ :+
  DEC cursor_row
:
  LDA pressed_buttons
  AND #BUTTON_DOWN
  BEQ :+
  LDA cursor_row
  CMP #4
  BEQ :+
  INC cursor_row
:
  LDA pressed_buttons
  AND #BUTTON_LEFT
  BEQ :+
  LDA cursor_column
  BEQ :+
  DEC cursor_column
:
  LDA pressed_buttons
  AND #BUTTON_RIGHT
  BEQ :+
  LDA cursor_column
  CMP #6
  BEQ :+
  INC cursor_column
:
  JSR render_cursor

  LDA pressed_buttons
  AND #BUTTON_A
  JSR activate_selection
  RTS
.endproc

.proc activate_selection
  LDA cursor_row
  ASL
  ASL
  ASL
  ORA cursor_column
  TAY
  LDA cursor_to_index, Y
  TAY
.endproc

.proc binary_button
  BRK ; not implemented
  RTS
.endproc

.proc unary_button
  BRK ; not implemented
  RTS
.endproc

.proc pct_button
  BRK ; not implemented
  RTS
.endproc

.proc clear_button
  BRK ; not implemented
  RTS
.endproc

.proc digit_button
  BRK ; not implemented
  RTS
.endproc

.proc decimal_button
  BRK ; not implemented
  RTS
.endproc

.proc delete_button
  BRK ; not implemented
  RTS
.endproc

.proc compute_button
  BRK ; not implemented
  RTS
.endproc

.proc mc_button
  BRK ; not implemented
  RTS
.endproc

.proc mr_button
  BRK ; not implemented
  RTS
.endproc

.proc mplus_button
  BRK ; not implemented
  RTS
.endproc

.proc mminus_button
  BRK ; not implemented
  RTS
.endproc

.proc render_cursor
  JSR reset_sprite_counter

  LDA cursor_row
  ASL
  ASL
  ASL
  ORA cursor_column
  TAY
  LDA cursor_to_index, Y
  TAY

  LDA cursor_x0, Y
  STA metasprite_x
  LDA cursor_y0, Y
  STA metasprite_y
  STY temp_y
  LDX #$00
  LDY #$01
  JSR draw_sprite
  LDY temp_y

  LDA cursor_x1, Y
  STA metasprite_x
  LDA cursor_y0, Y
  STA metasprite_y
  STY temp_y
  LDX #$00
  LDY #$01|OAM_FLIP_H
  JSR draw_sprite
  LDY temp_y

  LDA cursor_x0, Y
  STA metasprite_x
  LDA cursor_y1, Y
  STA metasprite_y
  STY temp_y
  LDX #$00
  LDY #$01|OAM_FLIP_V
  JSR draw_sprite
  LDY temp_y

  LDA cursor_x1, Y
  STA metasprite_x
  LDA cursor_y1, Y
  STA metasprite_y
  STY temp_y
  LDX #$00
  LDY #$01|OAM_FLIP_H|OAM_FLIP_V
  JSR draw_sprite
  LDY temp_y

  JSR erase_remaining_sprites
  RTS
.endproc

.proc clear_accumulator
  LDX #7
  LDA #$00
: STA accumulator, X
  DEX
  BPL :-
  STA operator
  RTS
.endproc

.proc clear_input
  LDX #7
  LDA #$00
: STA input, X
  DEX
  BPL :-
  STA decimal_point_active
  LDA #$01
  STA mantissa_nibble
  LDA #$00
  STA mantissa_digit

  JSR refresh_input
  RTS
.endproc

.macro push_upper_nibble value
  LDA value
  AND #$f0
  .repeat 4
    LSR
  .endrepeat
  TAY
  vb_push {metatile_l, Y}
  vb_push {metatile_r, Y}
.endmacro

.macro push_lower_nibble value
  LDA value
  AND #$0f
  TAY
  vb_push {metatile_l, Y}
  vb_push {metatile_r, Y}
.endmacro

.macro refresh_number_half number, half
  .if half = 0
    metatile_l = metatile_ul
    metatile_r = metatile_ur
    ppu_ptr = $a082
  .else
    metatile_l = metatile_dl
    metatile_r = metatile_dr
    ppu_ptr = $a0a2
  .endif

  .if number = 0
    value := input
  .else
    value := accumulator
  .endif

  vb_alloc {(2+14*2+2)}

  vb_ppu_addr ppu_ptr

  ; mantissa sign
  LDA value
  AND #%10000000
  BNE negative_mantissa

  vb_push {metatile_l+metatiles::empty}
  vb_push {metatile_r+metatiles::empty}
  JMP mantissa
negative_mantissa:
  vb_push {metatile_l+metatiles::negative}
  vb_push {metatile_r+metatiles::negative}
mantissa:

  push_upper_nibble {value+2}

  vb_push {metatile_l+metatiles::decimal}
  vb_push {metatile_r+metatiles::decimal}

  push_lower_nibble {value+2}

  ; skip last 2 numbers, they don't fit
  .repeat 3, index
    push_upper_nibble {value+3+index}
    push_lower_nibble {value+3+index}
  .endrepeat

  ; exponent sign
  LDA value
  AND #%01000000
  BNE negative_exponent

  vb_push {metatile_l+metatiles::empty}
  vb_push {metatile_r+metatiles::empty}
  JMP exponent
negative_exponent:
  vb_push {metatile_l+metatiles::negative}
  vb_push {metatile_r+metatiles::negative}
exponent:
  push_lower_nibble {value}
  push_upper_nibble {value+1}
  push_lower_nibble {value+1}

  vb_push #$ff
  vb_close
.endmacro

.proc refresh_input
  .scope upper
    refresh_number_half 0, 0
  .endscope
  .scope lower
    refresh_number_half 0, 1
  .endscope
  RTS
.endproc


.proc refresh_accumulator
  .scope upper
    refresh_number_half 1, 0
  .endscope
  .scope lower
    refresh_number_half 1, 1
  .endscope
  RTS
.endproc

.segment "RODATA"
nametable: .incbin "../assets/nametables/main.rle"

;                    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   -,   .,   _
metatile_ul: .byte $10, $01, $16, $16, $1c, $20, $20, $22, $20, $20, $23, $01, $01
metatile_ur: .byte $11, $14, $17, $17, $1d, $21, $21, $11, $17, $17, $24, $01, $01
metatile_dl: .byte $12, $01, $18, $1a, $1e, $1a, $18, $01, $18, $1a, $25, $27, $01
metatile_dr: .byte $13, $15, $19, $1b, $1f, $1b, $1b, $15, $1b, $1b, $26, $28, $01

cursor_to_index:
.repeat 5, row
  .repeat 8, column
    .if column = 7
      .byte $00
    .else
      .byte row * 7 + column
    .endif
  .endrepeat
.endrepeat

cursor_x0:
.repeat 5
  .byte $10, $40, $80, $98, $b0, $c8, $e0
.endrepeat
cursor_x1:
.repeat 5
  .byte $38, $68, $90, $a8, $c0, $d8, $f0
.endrepeat
cursor_y0:
.repeat 7
  .byte $40
.endrepeat
.repeat 7
  .byte $58
.endrepeat
.repeat 7
  .byte $70
.endrepeat
.repeat 7
  .byte $88
.endrepeat
.repeat 6
  .byte $a0
.endrepeat
.byte $88
cursor_y1:
.repeat 7
  .byte $50
.endrepeat
.repeat 7
  .byte $68
.endrepeat
.repeat 7
  .byte $80
.endrepeat
.repeat 6
  .byte $98
.endrepeat
.byte $b0
.repeat 7
  .byte $b0
.endrepeat

.linecont +
.define button_callbacks unary_button-1, \
                         binary_button-1, \
                         pct_button-1, \
                         clear_button-1, \
                         unary_button-1, \
                         binary_button-1, \
                         binary_button-1, \
                         unary_button-1, \
                         unary_button-1, \
                         mc_button-1, \
                         digit_button-1, \
                         digit_button-1, \
                         digit_button-1, \
                         binary_button-1, \
                         unary_button-1, \
                         unary_button-1, \
                         mr_button-1, \
                         digit_button-1, \
                         digit_button-1, \
                         digit_button-1, \
                         binary_button-1, \
                         unary_button-1, \
                         unary_button-1, \
                         mplus_button-1, \
                         digit_button-1, \
                         digit_button-1, \
                         digit_button-1, \
                         compute_button-1, \
                         unary_button-1, \
                         unary_button-1, \
                         mminus_button-1, \
                         digit_button-1, \
                         decimal_button-1, \
                         delete_button-1, \
                         compute_button-1
button_callbacks_l: .lobytes button_callbacks
button_callbacks_h: .hibytes button_callbacks

; layout
; exp pow   %  C +/- /  *
; sqr 1/x   mc 7  8  9  -
; sin cos   mr 4  5  6  +
; sec csc   m+ 1  2  3  =
; tan cot   m- 0  . del =
