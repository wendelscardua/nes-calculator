.ifndef _MATH_INC_
  _MATH_INC_ = 1

  .segment "ZEROPAGE"

  .import w1, w2, w3, w4

  .segment "CODE"

  .import calc
  .import copy2w

  .segment "RODATA"

  .import zero
  .import ln2
  .import ln10
  .import iln10
  .import exp0
  .import sqt2
  .import pi
  .import twopi
  .import sqp5
  .import invpi
  .import i2pi
  .import pio2
  .import npio2
  .import tpio2
  .import pio4
  .import unit
  .import half
  .import nrpi2
  .import hnth
  .import ovferr
  .import rngerr
  .import dmnerr
  .import dvzerr

.endif
