.include "constants.inc"
.include "nmi.inc"

.export vram_buffer
.export vram_buffer_sp
.export main_sp
.export flush_vram_buffer
.export _tens
.export _ones

.segment "ZEROPAGE"

vram_buffer_sp: .res 1
main_sp: .res 1
str_ptr: .res 2

.segment "STACK"
vram_buffer: .res 128

.segment "CODE"

.proc flush_vram_buffer
  LDA vram_buffer_sp
  BNE :+
  RTS
:
  TSX
  STX main_sp
  LDX #0
  STX vram_buffer_sp
  DEX
  TXS

  BIT PPUSTATUS
loop:
  PLA
  BEQ exit_loop
  CMP #$80
  BCS string
  CMP #$3f
  BEQ palette
  STA PPUADDR
  PLA
  STA PPUADDR
  PLA
  STA PPUDATA
  JMP loop
string:
  AND #%00111111
  STA PPUADDR
  PLA
  STA PPUADDR
strloop:
  PLA
  CMP #$ff
  BEQ loop
  STA PPUDATA
  JMP strloop
palette:
  STA PPUADDR
  PLA
  STA PPUADDR
mini_palette_loop:
  PLA
  BMI loop
  STA PPUDATA
  JMP mini_palette_loop
exit_loop:
  LDX main_sp
  TXS

  BIT PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  RTS
.endproc

.segment "RODATA"

_tens:
.repeat 100, i
  .byte (i / 10) + $10
.endrepeat
_ones:
.repeat 100, i
  .byte (i .mod 10) + $10
.endrepeat
