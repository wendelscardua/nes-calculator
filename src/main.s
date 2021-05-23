.include "audio-data.inc"
.include "calculator.inc"
.include "constants.inc"
.include "graphics.inc"
.include "irq.inc"
.include "macros.inc"
.include "nmi.inc"
.include "rand.inc"
.include "readjoy.inc"
.include "reset.inc"
.include "vram-buffer.inc"

.segment "CODE"

.export main
.proc main
  SEI         ; ignore IRQs
  CLD         ; disable decimal mode
  LDX #$40
  STX $4017   ; disable APU frame IRQ
  LDX #$ff
  TXS         ; Set up stack
  INX         ; now X = 0
  STX PPUCTRL ; disable NMI
  STX PPUMASK ; disable rendering
  STX $4010   ; disable DMC IRQs

  LDX #0
clear_ram:
  LDA #$00
  STA $0000,X
  STA $0100,X
  STA $0300,X
  STA $0400,X
  STA $0500,X
  STA $0600,X
  STA $0700,X
  LDA #$fe
  STA $0200,X
  INX
  BNE clear_ram

  reset_vram_stack

  ; rng seed
  LDA #$ca
  STA rng_seed
  LDA #$fe
  STA rng_seed+1

  SCREEN_ON

  JSR calculator_init

forever:
  LDA nmis
: CMP nmis
  BEQ :-
  ; new frame code
  .ifdef DEBUG
    LDA #%11011110  ; tint
    STA PPUMASK
  .endif
  JSR readjoy
  JSR input_handler
  JSR rand
  .ifdef DEBUG
    LDA #%00011110  ; no tint
    STA PPUMASK
  .endif
  JMP forever
.endproc

.proc input_handler
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler
