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

input_ptr: .res 2
memory: .res 8
mantissa_digit: .res 1
mantissa_nibble: .res 1
decimal_point_active: .res 1

inverse_and_hyperbolic_status: .res 1

cursor_row: .res 1
cursor_column: .res 1

.segment "BSS"

input_stack: .res 64

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

  JSR clear_stack

  LDA #$00
  STA inverse_and_hyperbolic_status

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

  JSR clear_input
  JSR refresh_display

  RTS
.endproc

.export calculator_io
.proc calculator_io
  LDA pressed_buttons
  AND #BUTTON_B
  BEQ :+
  ; TODO visual feedback
  LDA inverse_and_hyperbolic_status
  EOR #%10
  STA inverse_and_hyperbolic_status
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
  ; TODO toggling inv/hyp modes
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
  LDA inverse_and_hyperbolic_status
  BEQ normal
  CMP #%10
  BEQ inverse
  CMP #%01
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

.proc binary_button
  JSR get_operation
  PHA
  ; copy input to w2
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w2
  JSR copy2w


  ; pop stack
  JSR clear_input
  SEC
  LDA input_ptr
  SBC #8
  STA input_ptr
  LDA input_ptr+1
  SBC #0
  STA input_ptr+1

  ; copy input to w1
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w1
  JSR copy2w

  ; execute operation
  PLA
  TAX
  JSR calc
  ; copy w3 to input
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w3
  JSR copyw2
  JSR dirty_input
  JSR refresh_display
  RTS
.endproc

.proc unary_button
  JSR get_operation
  PHA
  ; copy input to w1
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w1
  JSR copy2w
  ; execute operation
  PLA
  TAX
  JSR calc
  ; copy w3 to input
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w3
  JSR copyw2
  JSR dirty_input
  JSR refresh_display
  RTS
.endproc

.proc pct_button ; computes current-value % of previous-value
  ; copy input to w1
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w1
  JSR copy2w

  ; copy 100 to w2
  LDA #<hundred
  LDY #>hundred
  LDX #<w2
  JSR copy2w

  LDX #operations::div
  JSR calc

  ; copy w3 to w1
  LDA #<w3
  LDY #>w3
  LDX #<w1
  JSR copy2w

  ; pop stack
  JSR clear_input
  SEC
  LDA input_ptr
  SBC #8
  STA input_ptr
  LDA input_ptr+1
  SBC #0
  STA input_ptr+1

  ; copy input to w2
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w2
  JSR copy2w

  LDX #operations::mul
  JSR calc

  ; copy w3 to input
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w3
  JSR copyw2
  JSR dirty_input
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
  AND #%10
  BEQ square
  JSR unary_button
  RTS
square:
  ; copy input to w1
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w1
  JSR copy2w
  ; copy input to w1
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w2
  JSR copy2w

  ; multiply
  LDX #operations::mul
  JSR calc

  ; copy w3 to input
  LDA input_ptr
  LDY input_ptr+1
  LDX #<w3
  JSR copyw2
  JSR dirty_input
  JSR refresh_display

  RTS
.endproc

.proc digit_button
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
  ORA (input_ptr), Y
  STA (input_ptr), Y

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
  LDA (input_ptr), Y
  bcd_adc #$01
  STA (input_ptr), Y
  DEY
  LDA (input_ptr), Y
  bcd_adc #$00
  STA (input_ptr), Y
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

.proc input_button ; pushes value to stack
  ; TODO: avoid stack overflow
  LDA input_ptr
  CLC
  ADC #8
  STA input_ptr
  BCC :+
  INC input_ptr+1
:
  JSR clear_input
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
  STA (input_ptr), Y
  DEY
  BPL :-
  JSR dirty_input
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
  LDA input_ptr
  LDY input_ptr+1
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
  LDA input_ptr
  LDY input_ptr+1
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

.proc clear_stack
  LDX #63
  LDA #0
: STA input_stack, X
  DEX
  BPL :-

  LDA #<input_stack
  STA input_ptr
  LDA #>input_stack
  STA input_ptr+1
  RTS
.endproc

.proc clear_input
  LDY #7
  LDA #$00
: STA (input_ptr), Y
  DEY
  BPL :-
  STA decimal_point_active
  LDA #$01
  STA mantissa_nibble
  LDA #$00
  STA mantissa_digit

  RTS
.endproc

.proc dirty_input ; move "cursor" to end of number (usually a result)
  LDA #$06
  STA mantissa_digit
  LDA #$00
  STA mantissa_nibble
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
  LDA (input_ptr), Y
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
  push_upper_nibble {(input_ptr), Y}

  vb_push {metatile_l+metatiles::decimal}
  vb_push {metatile_r+metatiles::decimal}

  push_lower_nibble {(input_ptr), Y}

  ; skip last 2 numbers, they don't fit
  .repeat 3
    INY
    push_upper_nibble {(input_ptr), Y}
    push_lower_nibble {(input_ptr), Y}
  .endrepeat

  ; exponent sign
  LDY #$00
  LDA (input_ptr), Y
  AND #%01000000
  BNE negative_exponent

  vb_push {metatile_l+metatiles::empty}
  vb_push {metatile_r+metatiles::empty}
  JMP exponent
negative_exponent:
  vb_push {metatile_l+metatiles::negative}
  vb_push {metatile_r+metatiles::negative}
exponent:
  push_lower_nibble {(input_ptr), Y}
  INY
  push_upper_nibble {(input_ptr), Y}
  push_lower_nibble {(input_ptr), Y}

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
                         input_button-1, \
                         unary_button-1, \
                         unary_button-1, \
                         mminus_button-1, \
                         digit_button-1, \
                         decimal_button-1, \
                         delete_button-1, \
                         input_button-1
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

hundred:   .byte $00,$02,$10,$00,$00,$00,$00,$00

; layout
; exp pow   %  C +/- /  *
; sqr 1/x   mc 7  8  9  -
; sin cos   mr 4  5  6  +
; sec csc   m+ 1  2  3  =
; tan cot   m- 0  . del =
