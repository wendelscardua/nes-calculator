.ifndef _CONFIG_INC_
  _CONFIG_INC_ = 1

  SCREEN_ON_FLAGS = %10001000 ; turn on nmis, sprites use second pattern table

  ; delay for fading
  FADE_DELAY = 4

  ; wait before moving a step
  TURTLE_WAIT_FRAMES = 16

  ; pixels per turtle step
  TURTLE_STEP_SIZE = 16

  ; wait before restart after dying
  DEATH_WAIT_FRAMES = 60

  ; wait before going to next level if celebrating
  VICTORY_WAIT_FRAMES = 120

  .feature force_range
.endif
