.ifndef _RAND_INC_
  _RAND_INC_ = 1

  .segment "ZEROPAGE"

  .import rng_seed

  .segment "CODE"

  .import rand

.endif
