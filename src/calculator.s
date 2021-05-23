.include "graphics.inc"
.include "math.inc"
.include "nmi.inc"
.include "unrle.inc"

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

  ; XXX testing
  lda #<pi
  ldy #>pi
  ldx #<w1
  jsr copy2w

  lda #<exp0
  ldy #>exp0
  ldx #<w2
  jsr copy2w

  ; bugged: mul, div, inv, frac, sin, cos, sec_, cot, pow, log10, log2, loge, exp
  ; untested: asin, acos, atan, acsc, asec, acot, sinh, cosh, tanh
  ; csch, sech, coth, asinh, acosh, atanh, acsch, asech, acoth
  ldx #operations::mul
  jsr calc
  ; 00 31 41 59 26 53 59 = pi
  ; 00 27 18 28 18 28 46 = e
  ; 00 85 39 73 42 22 67 = pi*e
  RTS
.endproc

.export calculator_io
.proc calculator_io
  RTS
.endproc

.segment "RODATA"

nametable: .incbin "../assets/nametables/main.rle"
