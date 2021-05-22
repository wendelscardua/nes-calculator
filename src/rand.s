.segment "ZEROPAGE"

.export rng_seed
rng_seed: .res 2

.segment "CODE"

.export rand
; galois16o, from https://github.com/bbbradsmith/prng_6502/blob/master/galois16.s
.proc rand
  LDA rng_seed+1
  TAY ; store copy of high byte
  ; compute rng_seed+1 ($39>>1 = %11100)
  LSR ; shift to consume zeroes on left...
  LSR
  LSR
  STA rng_seed+1 ; now recreate the remaining bits in reverse order... %111
  LSR
  EOR rng_seed+1
  LSR
  EOR rng_seed+1
  EOR rng_seed+0 ; recombine with original low byte
  STA rng_seed+1
  ; compute rng_seed+0 ($39 = %111001)
  TYA ; original high byte
  STA rng_seed+0
  ASL
  EOR rng_seed+0
  ASL
  EOR rng_seed+0
  ASL
  ASL
  ASL
  EOR rng_seed+0
  STA rng_seed+0
  RTS
.endproc
