.ifndef _MATH_INC_
  _MATH_INC_ = 1

  .include "math-defs.inc"

  .segment "MATH"

  .import w1, w2, w3, w4

  .enum operations
    add
    sub
    mul
    div
    sqrt
    inv
    int
    frac
    abs
    chs
    sin
    cos
    tan
    csc
    sec_
    cot
    asin
    acos
    atan
    acsc
    asec
    acot
    loge
    exp
    sinh
    cosh
    tanh
    csch
    sech
    coth
    asinh
    acosh
    atanh
    acsch
    asech
    acoth
    log2
    log10
    pow
  .endenum

  .segment "CODE"

  .import calc
  .import copy2w
  .import copyw2
  .import bcd_add
  .import bcd_sub

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
