.ifndef _MATH_DEFS_INC_
  _MATH_DEFS_INC_ = 1

  .macro bcd_adc param, indirect
    sta temp_x
    .ifnblank indirect
      lda param, indirect
    .else
      lda param
    .endif
    sta temp_y
    jsr bcd_add
  .endmacro

  .macro bcd_sbc param, indirect
    sta temp_x
    .ifnblank indirect
      lda param, indirect
    .else
      lda param
    .endif
    sta temp_y
    jsr bcd_sub
  .endmacro
.endif
