.ifndef _MACROS_INC_
  _MACROS_INC_ = 1

  .macpack longbranch

  .macro KIL ; pseudo instruction to kill the program
    .byte $12
  .endmacro

  .macro NEG ; negates A (cobbles carry)
    EOR #$FF
    SEC
    ADC #0
  .endmacro

  .macro JEQ label ; far jump version of BEQ
    .local skip
    BNE skip
    JMP label
skip:
  .endmacro

  .macro JNE label ; far jump version of BNE
    .local skip
    BEQ skip
    JMP label
skip:
  .endmacro

  .macro save_regs
    PHA
    TXA
    PHA
    TYA
    PHA
  .endmacro

  .macro restore_regs
    PLA
    TAY
    PLA
    TAX
    PLA
  .endmacro

  ; Sets N flag if NUM1 (16 bits) < NUM2 (16 bits)
  ; From http://www.6502.org/tutorials/compare_beyond.html#5
  .macro signed_compare_words NUM1H, NUM1L, NUM2H, NUM2L
    .local exit
    LDA NUM1L ; NUM1-NUM2
    CMP NUM2L
    LDA NUM1H
    SBC NUM2H
    BVC exit ; N eor V
    EOR #$80
exit:
  .endmacro

  .macro COPY_WORD src, dest
    LDA #<src
    STA dest
    LDA #>src
    STA dest+1
  .endmacro
.endif
