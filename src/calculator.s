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

old_input: .res 8
input: .res 8
memory: .res 8
mantissa_digit: .res 1
mantissa_nibble: .res 1
decimal_point_active: .res 1

inverse_and_hyperbolic_status: .res 1
pending_operation: .res 1

cursor_row: .res 1
cursor_column: .res 1

; an input is dirty if it's filled with a value - typing will clear it
dirty_input: .res 1

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

  LDA #$00
  STA inverse_and_hyperbolic_status

  JSR update_function_buttons

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

  LDA #$ff
  STA pending_operation

  JSR clear_old_input
  JSR clear_input
  JSR refresh_display

  RTS
.endproc

.export calculator_io
.proc calculator_io
  LDA pressed_buttons
  AND #BUTTON_B
  BEQ :+
  LDA inverse_and_hyperbolic_status
  CLC
  ADC #1
  AND #%11
  STA inverse_and_hyperbolic_status
  JSR update_function_buttons
:
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
  BEQ :+
  JSR activate_selection
:
  RTS
.endproc

.proc activate_selection
  LDA cursor_row
  ASL
  ASL
  ASL
  ORA cursor_column
  TAX
  LDA cursor_to_index, X
  TAX
  LDA button_callbacks_h, X
  PHA
  LDA button_callbacks_l, X
  PHA
  RTS
.endproc

.proc get_operation
  ; returns math operation for current button
  ; input: X = button as index
  LDA inverse_and_hyperbolic_status
  BEQ normal
  CMP #%01
  BEQ inverse
  CMP #%10
  BEQ hyperbolic
  CMP #%11
  BEQ inverse_hyperbolic
  BRK ; unexpected state
normal:
  LDA operation_per_index, X
  RTS
inverse:
  LDA inverse_operation_per_index, X
  RTS
hyperbolic:
  LDA hyperbolic_operation_per_index, X
  RTS
inverse_hyperbolic:
  LDA inverse_hyperbolic_operation_per_index, X
  RTS
.endproc

.proc calculate_button
  ; calculates pending binary operation
  LDA pending_operation
  CMP #$ff
  BNE :+
  ; no pending operation, return
  RTS
:
  ; copy old input to w1, new input to w2
  LDA #<old_input
  LDY #>old_input
  LDX #<w1
  JSR copy2w

  LDA #<input
  LDY #>input
  LDX #<w2
  JSR copy2w

  LDX pending_operation
  JSR calc

  ; copy from w3 to input
  LDA #<input
  LDY #>input
  LDX #<w3
  JSR copyw2

  LDA #1
  STA dirty_input

  JSR refresh_display

  LDA #$ff
  STA pending_operation

  RTS
.endproc

.proc binary_button
  LDA pending_operation
  CMP #$ff
  BEQ :+
  TXA
  PHA
  JSR calculate_button
  PLA
  TAX
:
  JSR get_operation
  STA pending_operation

  ; copy input to old input
  LDX #7
: LDA input, X
  STA old_input, X
  DEX
  BPL :-

  LDA #1
  STA dirty_input

  RTS
.endproc

.proc unary_button
  JSR get_operation
  PHA
  ; copy input to w1
  LDA #<input
  LDY #>input
  LDX #<w1
  JSR copy2w
  ; execute operation
  PLA
  TAX
  JSR calc
  ; copy w3 to input
  LDA #<input
  LDY #>input
  LDX #<w3
  JSR copyw2
  LDA #1
  STA dirty_input
  JSR refresh_display
  RTS
.endproc

.proc pct_button
  ; if pending add/subtract operation, input = input * 0.01 * old_input
  ; else input = input * 0.01

  ; copy input to w1
  LDA #<input
  LDY #>input
  LDX #<w1
  JSR copy2w

  ; copy hnth (1/100) to w2
  LDA #<hnth
  LDY #>hnth
  LDX #<w2
  JSR copy2w

  ; w3 = w1 * w2
  LDX #operations::mul
  JSR calc

  LDA pending_operation
  CMP #operations::add
  BEQ compute_relative
  CMP #operations::sub
  BEQ compute_relative
  JMP update_input
compute_relative:
  ; copy w3 to w1
  LDA #<w3
  LDY #>w3
  LDX #<w1
  JSR copy2w

  ; copy old input to w2
  LDA #<old_input
  LDY #>old_input
  LDX #<w2
  JSR copy2w

  LDX #operations::mul
  JSR calc

update_input:
  ; copy w3 to input
  LDA #<input
  LDY #>input
  LDX #<w3
  JSR copyw2

  LDA #1
  STA dirty_input
  JSR refresh_display

  RTS
.endproc

.proc clear_button
  JSR clear_input
  JSR refresh_display
  RTS
.endproc

.proc square_button
  LDA inverse_and_hyperbolic_status
  AND #%01
  BEQ square
  JSR unary_button
  RTS
square:
  ; copy input to w1
  LDA #<input
  LDY #>input
  LDX #<w1
  JSR copy2w
  ; copy input to w1
  LDA #<input
  LDY #>input
  LDX #<w2
  JSR copy2w

  ; multiply
  LDX #operations::mul
  JSR calc

  ; copy w3 to input
  LDA #<input
  LDY #>input
  LDX #<w3
  JSR copyw2
  LDA #1
  STA dirty_input
  JSR refresh_display

  RTS
.endproc

.proc digit_button
  LDA dirty_input
  BEQ :+
  JSR clear_input
:

  LDY mantissa_digit
  CPY #6
  BNE :+
  ; TODO maybe beep ?
  RTS
:
  LDA digit_per_index, X
  STA temp_x
  LDA mantissa_nibble
  BEQ :+
  .repeat 4
    ASL temp_x
  .endrepeat
: LDA temp_x
  INY
  INY
  ORA input, Y
  STA input, Y

  LDA mantissa_nibble
  BEQ :+
  DEC mantissa_nibble
  JMP update_exp
  RTS
:
  INC mantissa_nibble
  INC mantissa_digit
update_exp:
  LDA decimal_point_active
  BNE :+

  LDA mantissa_digit
  BEQ :+

  SED
  CLC
  LDY #$01
  LDA input, Y
  bcd_adc #$01
  STA input, Y
  DEY
  LDA input, Y
  bcd_adc #$00
  STA input, Y
  CLD

:
  JSR refresh_display
  RTS
.endproc

.proc decimal_button
  LDA #$01
  STA decimal_point_active
  RTS
.endproc

.proc delete_button
  BRK ; not implemented
  RTS
.endproc

.proc mc_button
  LDY #7
  LDA #0
: STA memory, Y
  DEY
  BPL :-
  RTS
.endproc

.proc mr_button
  LDY #7
: LDA memory, Y
  STA input, Y
  DEY
  BPL :-
  LDA #1
  STA dirty_input
  JSR refresh_display
  RTS
.endproc

.proc mplus_button
  ; copy memory to w1
  LDA #<memory
  LDY #>memory
  LDX #<w1
  JSR copy2w

  ; copy input to w2
  LDA #<input
  LDY #>input
  LDX #<w2
  JSR copy2w

  LDX #operations::add
  JSR calc

  ; copy w3 to memory
  LDA #<memory
  LDY #>memory
  LDX #<w3
  JSR copyw2
  RTS
.endproc

.proc mminus_button
  ; copy memory to w1
  LDA #<memory
  LDY #>memory
  LDX #<w1
  JSR copy2w

  ; copy input to w2
  LDA #<input
  LDY #>input
  LDX #<w2
  JSR copy2w

  LDX #operations::sub
  JSR calc

  ; copy w3 to memory
  LDA #<memory
  LDY #>memory
  LDX #<w3
  JSR copyw2
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

.proc clear_input
  ; TODO implement All Clear as well
  LDY #7
  LDA #$00
: STA input, Y
  DEY
  BPL :-
  STA decimal_point_active
  LDA #$01
  STA mantissa_nibble
  LDA #$00
  STA mantissa_digit
  STA dirty_input
  RTS
.endproc

.proc clear_old_input
  LDY #7
  LDA #$00
: STA old_input, Y
  DEY
  BPL :-
  RTS
.endproc

.macro push_upper_nibble value
  TYA
  PHA
  LDA value
  AND #$f0
  .repeat 4
    LSR
  .endrepeat
  TAY
  vb_push {metatile_l, Y}
  vb_push {metatile_r, Y}
  PLA
  TAY
.endmacro

.macro push_lower_nibble value
  TYA
  PHA
  LDA value
  AND #$0f
  TAY
  vb_push {metatile_l, Y}
  vb_push {metatile_r, Y}
  PLA
  TAY
.endmacro

.macro refresh_number_half half
  .if half = 0
    metatile_l = metatile_ul
    metatile_r = metatile_ur
    ppu_ptr = $a082
  .else
    metatile_l = metatile_dl
    metatile_r = metatile_dr
    ppu_ptr = $a0a2
  .endif

  vb_alloc {(2+14*2+2)}

  vb_ppu_addr ppu_ptr

  ; mantissa sign
  LDY #$00
  LDA input, Y
  AND #%10000000
  BNE negative_mantissa

  vb_push {metatile_l+metatiles::empty}
  vb_push {metatile_r+metatiles::empty}
  JMP mantissa
negative_mantissa:
  vb_push {metatile_l+metatiles::negative}
  vb_push {metatile_r+metatiles::negative}
mantissa:

  LDY #$02
  push_upper_nibble {input, Y}

  vb_push {metatile_l+metatiles::decimal}
  vb_push {metatile_r+metatiles::decimal}

  push_lower_nibble {input, Y}

  ; skip last 2 numbers, they don't fit
  .repeat 3
    INY
    push_upper_nibble {input, Y}
    push_lower_nibble {input, Y}
  .endrepeat

  ; exponent sign
  LDY #$00
  LDA input, Y
  AND #%01000000
  BNE negative_exponent

  vb_push {metatile_l+metatiles::empty}
  vb_push {metatile_r+metatiles::empty}
  JMP exponent
negative_exponent:
  vb_push {metatile_l+metatiles::negative}
  vb_push {metatile_r+metatiles::negative}
exponent:
  push_lower_nibble {input, Y}
  INY
  push_upper_nibble {input, Y}
  push_lower_nibble {input, Y}

  vb_push #$ff
  vb_close
.endmacro

.proc refresh_display
  .scope upper
    refresh_number_half 0
  .endscope
  .scope lower
    refresh_number_half 1
  .endscope

  LDA input
  AND #%00100000 ; bit 5 tells if there was an error
  BEQ :+
  JSR error_handler
: RTS
.endproc

.proc error_handler
  LDA #1
  STA dirty_input

  LDA #$20
  STA ppu_addr+1
  LDA #$82
  STA ppu_addr

  LDA #22
  STA subscreen_width
  LDA #2
  STA subscreen_height

  LDA #<lcd_error
  STA subscreen_source_addr
  LDA #>lcd_error
  STA subscreen_source_addr+1

  JSR draw_subscreen

  RTS
.endproc

.proc update_function_buttons
  ; render exp/sqr or log/sqrt buttons
  LDA #$21
  STA ppu_addr+1
  LDA #$02
  STA ppu_addr
  LDA #5
  STA subscreen_width
  STA subscreen_height
  LDA inverse_and_hyperbolic_status
  AND #%01
  BNE :+
  LDA #<exp_sqr_buttons
  STA subscreen_source_addr
  LDA #>exp_sqr_buttons
  STA subscreen_source_addr+1
  JMP render_first_set
:
  LDA #<log_sqrt_buttons
  STA subscreen_source_addr
  LDA #>log_sqrt_buttons
  STA subscreen_source_addr+1
render_first_set:
  JSR draw_subscreen

  LDA #$21
  STA ppu_addr+1
  LDA #$c2
  STA ppu_addr
  LDA #11
  STA subscreen_width
  LDA #8
  STA subscreen_height

  LDA inverse_and_hyperbolic_status
  BNE :+
  LDA #<trig_buttons
  STA subscreen_source_addr
  LDA #>trig_buttons
  STA subscreen_source_addr+1
  JMP render_second_set
: CMP #%01
  BNE :+
  LDA #<inv_trig_buttons
  STA subscreen_source_addr
  LDA #>inv_trig_buttons
  STA subscreen_source_addr+1
  JMP render_second_set
: CMP #%10
  BNE :+
  LDA #<hyp_trig_buttons
  STA subscreen_source_addr
  LDA #>hyp_trig_buttons
  STA subscreen_source_addr+1
  JMP render_second_set
: LDA #<inv_hyp_trig_buttons
  STA subscreen_source_addr
  LDA #>inv_hyp_trig_buttons
  STA subscreen_source_addr+1

render_second_set:
  JSR draw_subscreen

  RTS
.endproc

.segment "RODATA"
nametable: .incbin "../assets/nametables/main.rle"

;                    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   -,   .,   _
metatile_ul: .byte $10, $01, $16, $16, $1c, $20, $20, $22, $20, $20, $23, $01, $01
metatile_ur: .byte $11, $14, $17, $17, $1d, $21, $21, $11, $17, $17, $24, $01, $01
metatile_dl: .byte $12, $01, $18, $1a, $1e, $1a, $18, $01, $18, $1a, $1e, $27, $01
metatile_dr: .byte $13, $15, $19, $1b, $1f, $1b, $1b, $15, $1b, $1b, $26, $25, $01

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
                         square_button-1, \
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
                         calculate_button-1, \
                         unary_button-1, \
                         unary_button-1, \
                         mminus_button-1, \
                         digit_button-1, \
                         decimal_button-1, \
                         delete_button-1, \
                         calculate_button-1
button_callbacks_l: .lobytes button_callbacks
button_callbacks_h: .hibytes button_callbacks

digit_per_index:
.byte 0, 0, 0, 0, 0, 0, 0
.byte 0, 0, 0, 7, 8, 9, 0
.byte 0, 0, 0, 4, 5, 6, 0
.byte 0, 0, 0, 1, 2, 3, 0
.byte 0, 0, 0, 0, 0, 0, 0

operation_per_index:
.byte operations::exp, operations::pow, 0, 0, operations::chs, operations::div, operations::mul
.byte 0, operations::inv, 0, 0, 0, 0, operations::sub
.byte operations::sin, operations::cos, 0, 0, 0, 0, operations::add
.byte operations::sec_, operations::csc, 0, 0, 0, 0, 0
.byte operations::tan, operations::cot, 0, 0, 0, 0, 0

inverse_operation_per_index:
.byte operations::loge, operations::pow, 0, 0, operations::chs, operations::div, operations::mul
.byte operations::sqrt, operations::inv, 0, 0, 0, 0, operations::sub
.byte operations::asin, operations::acos, 0, 0, 0, 0, operations::add
.byte operations::asec, operations::acsc, 0, 0, 0, 0, 0
.byte operations::atan, operations::acot, 0, 0, 0, 0, 0

hyperbolic_operation_per_index:
.byte operations::exp, operations::pow, 0, 0, operations::chs, operations::div, operations::mul
.byte 0, operations::inv, 0, 0, 0, 0, operations::sub
.byte operations::sinh, operations::cosh, 0, 0, 0, 0, operations::add
.byte operations::sech, operations::csch, 0, 0, 0, 0, 0
.byte operations::tanh, operations::coth, 0, 0, 0, 0, 0

inverse_hyperbolic_operation_per_index:
.byte operations::loge, operations::pow, 0, 0, operations::chs, operations::div, operations::mul
.byte operations::sqrt, operations::inv, 0, 0, 0, 0, operations::sub
.byte operations::asinh, operations::acosh, 0, 0, 0, 0, operations::add
.byte operations::asech, operations::acsch, 0, 0, 0, 0, 0
.byte operations::atanh, operations::acoth, 0, 0, 0, 0, 0

exp_sqr_buttons: ; 5x5
.byte $02,$81,$82,$83,$02
.byte $02,$84,$85,$86,$02
.byte $eb,$ec,$ec,$ec,$ec
.byte $02,$73,$78,$79,$02
.byte $02,$76,$7a,$7b,$02

log_sqrt_buttons: ; 5x5
.byte $02,$87,$88,$56,$02
.byte $02,$89,$8a,$8b,$02
.byte $eb,$ec,$ec,$ec,$ec
.byte $02,$8c,$8d,$8e,$02
.byte $02,$8f,$90,$91,$02

trig_buttons: ; 11 x 8
.byte $02,$92,$93,$94,$02,$e9,$02,$55,$98,$99,$02
.byte $02,$95,$96,$97,$02,$ea,$02,$a2,$a3,$a4,$02
.byte $eb,$ec,$ec,$ec,$ec,$ed,$eb,$ec,$ec,$ec,$ec
.byte $02,$55,$98,$99,$02,$e9,$02,$55,$a5,$99,$02
.byte $02,$9a,$9b,$9c,$02,$ea,$02,$a2,$a7,$9c,$02
.byte $eb,$ec,$ec,$ec,$ec,$ed,$eb,$ec,$ec,$ec,$ec
.byte $02,$9d,$9e,$83,$02,$e9,$02,$55,$9e,$a6,$02
.byte $02,$9f,$a0,$a1,$02,$ea,$02,$a2,$a8,$a9,$02

inv_trig_buttons: ; 11 x 8
.byte $02,$aa,$ab,$99,$02,$e9,$02,$ac,$ad,$ae,$02
.byte $02,$b1,$b2,$b3,$02,$ea,$02,$bb,$bc,$bd,$02
.byte $eb,$ec,$ec,$ec,$ec,$ed,$eb,$ec,$ec,$ec,$ec
.byte $02,$ac,$ad,$ae,$02,$e9,$02,$ac,$ad,$ba,$02
.byte $02,$b4,$b5,$b6,$02,$ea,$02,$bb,$be,$bf,$02
.byte $eb,$ec,$ec,$ec,$ec,$ed,$eb,$ec,$ec,$ec,$ec
.byte $02,$af,$b0,$a5,$02,$e9,$02,$ac,$ad,$c0,$02
.byte $02,$b7,$b8,$b9,$02,$ea,$02,$bb,$bc,$c1,$02

hyp_trig_buttons: ; 11 x 8
.byte $02,$c2,$c3,$c4,$02,$e9,$02,$c5,$c6,$c7,$02
.byte $02,$cb,$b9,$b3,$02,$ea,$02,$d1,$d2,$96,$02
.byte $eb,$ec,$ec,$ec,$ec,$ed,$eb,$ec,$ec,$ec,$ec
.byte $02,$c5,$c6,$c7,$02,$e9,$02,$c5,$ad,$c7,$02
.byte $02,$cc,$cd,$ce,$02,$ea,$02,$d3,$d4,$ce,$02
.byte $eb,$ec,$ec,$ec,$ec,$ed,$eb,$ec,$ec,$ec,$ec
.byte $02,$c8,$c9,$ca,$02,$e9,$02,$c5,$d5,$c7,$02
.byte $02,$cf,$d0,$b9,$02,$ea,$02,$d1,$d6,$d7,$02

inv_hyp_trig_buttons: ; 11 x 8
.byte $d8,$c6,$93,$d9,$02,$e9,$de,$df,$98,$e0,$94
.byte $da,$db,$96,$dc,$dd,$ea,$67,$e7,$a3,$b2,$97
.byte $eb,$ec,$ec,$ec,$ec,$ed,$eb,$ec,$ec,$ec,$ec
.byte $de,$df,$98,$e0,$94,$e9,$de,$df,$a5,$e0,$94
.byte $67,$e3,$9b,$e4,$97,$ea,$67,$e7,$a7,$e4,$97
.byte $eb,$ec,$ec,$ec,$ec,$ed,$eb,$ec,$ec,$ec,$ec
.byte $de,$e1,$9e,$e2,$94,$e9,$de,$df,$9e,$e6,$94
.byte $67,$e5,$a0,$b9,$97,$ea,$67,$e7,$a8,$e8,$97

lcd_error: ; 22x2
.byte $01,$01,$20,$21,$23,$24,$23,$24,$23,$24,$23,$24,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$18,$19,$4d,$26,$4d,$26,$18,$1b,$4d,$26,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01



; layout
; exp pow   %  C +/- /  *
; sqr 1/x   mc 7  8  9  -
; sin cos   mr 4  5  6  +
; sec csc   m+ 1  2  3  =
; tan cot   m- 0  . del =
