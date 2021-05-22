.ifndef _CONSTANTS_INC_
  _CONSTANTS_INC_ = 1

  PPUCTRL   = $2000
  PPUMASK   = $2001
  PPUSTATUS = $2002
  PPUSCROLL = $2005
  PPUADDR   = $2006
  PPUDATA   = $2007
  OAMADDR   = $2003
  OAMDMA    = $4014
  JOYPAD1   = $4016
  JOYPAD2   = $4017

  ;; Joypad buttons
  BUTTON_A      = 1 << 7
  BUTTON_B      = 1 << 6
  BUTTON_SELECT = 1 << 5
  BUTTON_START  = 1 << 4
  BUTTON_UP     = 1 << 3
  BUTTON_DOWN   = 1 << 2
  BUTTON_LEFT   = 1 << 1
  BUTTON_RIGHT  = 1 << 0

.endif