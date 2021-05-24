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

  ldx #operations::acot
  jsr calc
  RTS
.endproc

.export calculator_io
.proc calculator_io
  RTS
.endproc

.segment "RODATA"
nametable: .incbin "../assets/nametables/main.rle"

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

;                    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   -,   .,   _
metatile_ul: .byte $10, $01, $16, $16, $1c, $20, $20, $22, $20, $20, $23, $01, $01
metatile_ur: .byte $11, $14, $17, $17, $1d, $21, $21, $11, $17, $17, $24, $01, $01
metatile_dl: .byte $12, $01, $18, $1a, $1e, $1a, $18, $01, $18, $1a, $25, $27, $01
metatile_dr: .byte $13, $15, $19, $1b, $1f, $1b, $1b, $15, $1b, $1b, $26, $28, $01
