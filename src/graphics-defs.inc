.ifndef _GRAPHICS_DEFS_INC_
  _GRAPHICS_DEFS_INC_ = 1

  .include "config.inc"

  .enum metasprites
    up_turtle
    down_turtle
    left_turtle
    right_turtle
    strawberry
  .endenum

  .include "constants.inc"

  ;; Sprites
  OAM_FLIP_V = $80
  OAM_FLIP_H = $40
  OAM_BEHIND = $20

  MAX_SPRITES = $f0

  ;; Palettes
  BG0_PAL = $3f01
  BG1_PAL = $3f05
  BG2_PAL = $3f09
  BG3_PAL = $3f0d
  SP0_PAL = $3f11
  SP1_PAL = $3f15
  SP2_PAL = $3f19
  SP3_PAL = $3f1d

  .macro NAMETABLE address
    BIT PPUSTATUS
    LDA #>address
    STA PPUADDR
    LDA #<address
    STA PPUADDR
  .endmacro

  .macro SCREEN_OFF
    LDA #$00
    STA PPUCTRL ; disable NMI
    STA PPUMASK ; disable rendering
  .endmacro

  .macro SCREEN_ON
    LDA #SCREEN_ON_FLAGS
    STA PPUCTRL
    LDA #%00011110  ; turn on screen
    STA PPUMASK
  .endmacro

  .macro VBLANK
    .local vblankwait
vblankwait:
    BIT PPUSTATUS
    BPL vblankwait
  .endmacro
.endif
