.include "constants.inc"
.include "famitone5.inc"
.include "graphics.inc"
.include "macros.inc"
.include "readjoy.inc"
.include "vram-buffer.inc"

.segment "ZEROPAGE"
.exportzp nmis
nmis: .res 1

.segment "CODE"
.export nmi_handler
.proc nmi_handler
  save_regs
  JSR flush_vram_buffer
  INC nmis
  ; reset ppuaddr
  BIT PPUSTATUS
  JSR set_scroll
  JSR refresh_oam
  JSR FamiToneUpdate
  restore_regs
  RTI
.endproc

.export wait_frames
; input: X = number of frames to wait
.proc wait_frames
  LDA nmis
: CMP nmis
  BEQ :-
  DEX
  BNE wait_frames
  RTS
.endproc

.export skippable_wait_frames
.proc skippable_wait_frames
  LDA nmis
: CMP nmis
  BEQ :-
  JSR readjoy
  LDA buttons
  BNE :+
  DEX
  BNE wait_frames
: RTS
.endproc
