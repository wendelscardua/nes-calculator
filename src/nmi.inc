.ifndef _NMI_INC_
  _NMI_INC_ = 1

  .segment "ZEROPAGE"

  .import nmis

  .segment "CODE"

  .import nmi_handler
  .import wait_frames
  .import skippable_wait_frames
.endif
