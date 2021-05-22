.ifndef _READJOY_INC_
  _READJOY_INC_ = 1

  .segment "ZEROPAGE"

  .import buttons
  .import last_frame_buttons
  .import released_buttons
  .import pressed_buttons

  .segment "CODE"

  .import readjoy

.endif
