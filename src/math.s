;   fltpt7.cba -- floating point routines for 650X
;
;   (C) 1999 - 2008, C. Bond. All rights reserved.
;   Ported to ca65 by W. Scardua
;
;   v.1
;   This version includes add, subtract, multiply, divide, square
;   root, tangent and arctangent. The tangent and arctangent are
;   implemented as efficient BCD CORDIC algorithms. Sine, cosine
;   arcsine and arccosine are also provided.
;
;   v.2
;   This version is a massive rewrite of the previous version to
;   simplify and improve the algoritms and structure of the trig
;   functions.
;
;   v.3
;   Added remaining trig and inverse trig functions along with
;   natural log (ln) and exponential (exp).
;
;   v.4
;   Improved error reporting and added hyperbolic and inverse
;   hyperbolic functions.
;
;   v.5
;   Added log2, log10 and power (pow) function.
;
;   Todo:
;       1. Provide better documentation for all code,
;       2. Remove unused code and unreferenced labels.
;
;--------------------------------------------------------------------------
;
;   The internal 8-byte floating point format is as follows:
;
;
;       byte:   0   1   2   3   4   5   6   7
;               В   В   В   В   В   В   В   В
;               і   і   і   і   і   і   і   і
;               і   і   АДДДБДДДБДДДБДДДБДДДБДДДДД Mantissa (12 BCD DIGITS)
;               і   і  D.D  DD  DD  DD  DD  DD
;               і   і   і
;               і   і   АДДДДДДДДДДДДДДДДДДДДДДДДД (implied decimal point)
;               і   і
;               і   АДДДДДДДДДДДДДДДДДДДДДДДВДВДДД Middle and low
;               і                           і і    exponent digits
;     bits: 7654і3210                       і і
;           ВВXXБВВВВ                     E E EДДД Exponent
;           іііі іііі                     і
;           іііі іііі                     і
;           іііі АБББДДДДДДДДДДДДДДДДДДДДДБДДДДДД- High exponent
;           іііі                                   digit
;           ііАБДД Error reporting flags
;           іАДДДД Exponent Sign
;           АДДДДД Mantissa Sign
;
;
;   Any number represented in this format can also be represented in
;   the following display format:
;
;           (сSm)D.DDDDDDDDDDD(сSe)EEE
;
;   where the 'D's stand for a decimal mantissa digit and the 'E's
;   stand for decimal exponent digits. 'Sm' stands for a single bit
;   mantissa sign ( 0 = +, 1 = - ), and 'Se' stands the sign of the
;   exponent.
;
;   A convention observed in the following code is that a test for zero
;   can be done by simply testing the third byte of the number for
;   zero. This works because for any non-zero number the mantissa is
;   left justified until its two most significant digits are in this
;   location. Only zero (or an equivalent caused by underflow) will
;   have this location empty.
;
;   There are two unused bits in this storage format. Bits 4 and 5 of
;   the first byte (see above) are not assigned. One possible assignment
;   strategy uses these bits to identify invalid arguments. In this
;   version we have begun to implement it. We use the following format
;   for byte 0:
;
;                    x x     x x x x
;           byte 0:  - - 5 4 - - - -
;
;                        0 0            Valid number
;                        0 1            Underflow (not reported)
;                        1 0            Overflow
;                        1 1            Signal  (domain, range, ... error)
;
;   where the 'signal' is expressed by a BCD number in the exponent
;   position. When an invalid argument is detected, the
;   called routine will return with one of the following 8-byte strings:
;
;           $29,$99,$99,$99,$99,$99,$99,$99 : Overflow error
;           $30,1,0,0,0,0,0,0               : Range error
;           $30,2,0,0,0,0,0,0               : Domain error
;           $30,3,0,0,0,0,0,0               : Divide-by-zero error
;
;   The order of the two arguments in binary operations is given by:
;
;
;   add:    w1 + w2 -> w3
;   sub:    w1 - w2 -> w3
;   mul:    w1 * w2 -> w3
;   div:    w1 / w2 -> w3
;   pow:    w1 ^ w2 -> w3
;
;   For unary operations, the argument is in 'w1' and the result in 'w3':
;
;   sqrt:      w1   -> w3
;   int:       w1   -> w3
;   frac:      w1   -> w3
;   abs:       w1   -> w3
;   sine:      w1   -> w3
;    ...
;
;-------------------------------------------------------------------------
;
;   zero page assignments
;

.include "temps.inc"

.segment "ZEROPAGE"

tmp1: .res 1
tmp2: .res 1
dadj: .res 1
ctr1: .res 1
ctr2: .res 1
ctr3: .res 1
pctr: .res 1
bctr: .res 1
tnsgn: .res 1
qdrnt: .res 1
cotflg: .res 1
cosflg: .res 1
sinflg: .res 1
expflg: .res 1
mptr: .res 2
ptr1: .res 2
ptr2: .res 2
ptr3: .res 2
pptr: .res 2
;
;   misc equates
;
reglen = 8
wreglen = 16

.segment "CODE"
;
;
;   The following is a simple calculator interface which takes a
;   function number in the CPU 'x-reg' and executes the function
;   using arguments previously placed in the 'w1' and/or 'w2'
;   memory registers.
;
;   The 'x' register
;   should contain the function number as shown in the jump table
;   and the arguments should have been placed in 'w1' and/or 'w2'.
;   Return to the caller is done by executing a 'RTS' instruction
;   at the conclusion of the routine, or on encountering a fatal
;   error. The result (in 'w3') is either the desired function or
;   an error code.
;
.export calc
.proc calc
  ;
  ;   x-reg has an index into the function jsr table.
  ;
  lda jtbl_h, X
  pha
  lda jtbl_l, X
  pha
  rts
.endproc

.segment "RODATA"
;
;   jump table for functions.
;
.linecont +
.define jtbl add-1, \
             sub-1, \
             mul-1, \
             div-1, \
             sqrt-1, \
             inv-1, \
             int-1, \
             frac-1, \
             abs-1, \
             chs-1, \
             sin-1, \
             cos-1, \
             tan-1, \
             csc-1, \
             sec_-1, \
             cot-1, \
             asin-1, \
             acos-1, \
             atan-1, \
             acsc-1, \
             asec-1, \
             acot-1, \
             loge-1, \
             exp-1, \
             sinh-1, \
             cosh-1, \
             tanh-1, \
             csch-1, \
             sech-1, \
             coth-1, \
             asinh-1, \
             acosh-1, \
             atanh-1, \
             acsch-1, \
             asech-1, \
             acoth-1, \
             log2-1, \
             log10-1, \
             pow-1

jtbl_l: .lobytes jtbl
jtbl_h: .hibytes jtbl

;   WS TODO: wth is this?
;   Test for polynomial function evaluator
;
;    ldx #7
;:  lda zero,x       ; move input value to argument register
;    sta arg,x
;    dex
;    bpl :-
;    lda #5          ; set coefficient count
;    sta pctr
;    lda <kcos       ; setup pointer to coefficient list
;    sta pptr
;    lda #>kcos
;    sta pptr+1
;    jsr poly        ; and go.
;    brk


.segment "MATH"

.export w1, w2, w3, w4

ra: .res 16              ; These registers are for mantissa operations.
rb: .res 16
rc: .res 16
rd: .res 16
re: .res 16
rf: .res 16
;
w1: .res 8               ; These registers are for input of numbers in
w2: .res 8               ; storage format. 'w1' and/or 'w2' should be
w3: .res 8               ; loaded with the operands before calling
w4: .res 8               ; F.P. routines.  'w3' is the result.
scalereg: .res 8         ; register for scaling parameter
arg:  .res 8             ; argument register for poly routine
reg1: .res 8             ; temporary storage registers
reg2: .res 8
reg3: .res 8
reg4: .res 8

.segment "CODE"

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

.proc bcd_add
  php
  stx temp_z ; save x
  tsx
  lda $101, x
  and #%1000 ; decimal flag
  bne bcd_mode
  ; bin mode
  ldx temp_z ; restore x
  plp
  lda temp_x
  adc temp_y
  rts

bcd_mode:
  ldx temp_z ; restore x
  plp
  sty temp_z
  ldy temp_x
  lda dec2bin, y
  sta temp_x
  ldy temp_y
  lda dec2bin, y
  sta temp_y
  lda temp_x
  adc temp_y
  sta temp_x
  cmp #100
  bcs over
  lda #$0
  sta temp_y
  jmp ret
over:
  sec
  sbc #100
  sta temp_x
  lda #$1
  sta temp_y
ret:
  ldy temp_x
  lda bin2dec, y
  sta temp_x
  ldy temp_z
  lda temp_y
  cmp #$1
  lda temp_x
  rts
.endproc

.proc bcd_sub
  php
  stx temp_z ; save x
  tsx
  lda $101, x
  and #%1000 ; decimal flag
  bne bcd_mode
  ; bin mode
  ldx temp_z ; restore x
  plp
  lda temp_x
  sbc temp_y
  rts

bcd_mode:
  ldx temp_z ; restore x
  plp

  sty temp_z
  ldy temp_x
  lda dec2bin, y
  sta temp_x
  ldy temp_y
  lda dec2bin, y
  sta temp_y
  lda temp_x
  sbc temp_y
  sta temp_x
  bmi under
  lda #$1
  sta temp_y
  jmp ret
under:
  clc
  adc #100
  sta temp_x
  lda #$0
  sta temp_y
ret:
  ldy temp_x
  lda bin2dec, y
  sta temp_x
  ldy temp_z
  lda temp_y
  cmp #$1
  lda temp_x
  rts
.endproc

;
;   main floating point math package
;
.export sub
.proc sub
  lda w2          ; perform subtraction by changing sign
  eor #$80        ;  of second number and adding.
  sta w2
  ; XXX: leak to add
.endproc

.proc add
  sed
  lda w1+2        ; filter out case where one number is zero.
  bne add1
  lda #<w2
  ldy #>w2
  ldx #<w3         ; this convention references the low byte.
  jmp copy2w      ; w1 is zero -- return with w2 (which may also be zero).
add1:
  lda w2+2
  bne add2
  lda #<w1
  ldy #>w1
  ldx #<w3
  jmp copy2w      ; w2 is zero -- return with w1 (which won't be zero).
  ;
  ;   At this point, additions and subtractions involving '0.0' have
  ;   been handled. Both operands are now known to be non-zero.
  ;
add2:
  lda w1         ; determine if a sum or difference is required.
  eor w2
  bpl add3        ; branch if signs are the same, drop through otherwise
  ;
  ;  On entry, we only know that the two numbers have opposite
  ;  signs (mantissa signs). To subtract, we will determine
  ;  the larger of the two numbers (in magnitude) and place it
  ;  in 'ra'. The second number will be decimal aligned and
  ;  placed in 'rb'.
  ;
  ;  The sign of the result will carry the sign of the number
  ;  in 'ra' (largest in magnitude).
  ;
sub1:
  bit w1          ; now estimate which number is larger from tests
  bvs est1        ;  on the exponents.  If exponent signs are unequal,
  bit w2          ;  the positive one goes with the larger magnitude.
  bvs est2        ;
  bvc sbpe        ; both numbers have positive exponents.
est1:
  bit w2
  bvs sbne        ; both numbers have negative exponents.
  jsr xw12        ;
est2:
  jsr sumexp     ; exponents have opposite signs -- e1 is larger.
  lda tmp1
  beq est3
est2a:
  lda #<w1       ; e1 #>> e2
  ldy #>w1         ; just copy e1 to w3...
  ldx #<w3
  jmp copy2w      ; ...and exit.
est3:
  lda tmp2
  cmp #$12
  bcs est2a       ; e1 #>> e2
  lda #0         ; we now know that a subtraction will take place.
  sta dadj        ; initialize decimal adjust.
  jsr w12rab      ; put 'w1' and 'w2' into 'ra' and 'rb' respectively.
  jsr subrarb
  bcs est4a
  jsr xw12
  lda #0
  sta dadj
  jsr w12rab
  jsr subrarb
est4a:
  lda w1          ; 'tmp1' is used as a flag to identify the caller...
  eor #$40
  sta tmp1
  jsr rbtow3      ; ...of this subroutine.
  rts
sbpe:
  jsr diffexp     ; carry e1 #>= e2.
  bcs sbp1
  jsr xw12
sbp1:
  lda tmp1
  cmp #0
  bne est2a
  jmp est3
sbne:
  jsr diffexp     ; exponent diff: tmp1(hi),tmp2(lo), carry: e1 #>= e2.
  bcc sbp1
  jsr xw12
  lda tmp1
  jmp sbp1
  ;
  ;   We now have two numbers with the same sign.
  ;   Add two positive or negative numbers.
  ;   Handle sign on exit, as it does not affect the addition.
  ;
  ;   The following algorithm places the larger of the two
  ;   numbers in 'ra' and the other in 'rb', but only if a
  ;   decimal point adjustment is needed before the addition.
  ;
add3:
  bit w1          ; now estimate which number is larger from tests
  bvs et1         ;  on the exponents.  If exponent signs are unequal,
  bit w2          ;  the positive one goes with the larger magnitude.
  bvs et2         ;
  bvc adpe        ; both numbers have positive exponents.
et1:
  bit w2
  bvs adne        ; both numbers have negative exponents.
  jsr xw12        ; swap w1 and w2 so largest magnitude is in w1.
et2:
  jsr sumexp     ; add the exponents to determine the decimal
  ;  point adjustment to be applied to w2.
  lda tmp1        ; if tmp1 #<> 0 the sum exceeds number of digits
  beq et3
et2a:
  lda #<w1
  ldy #>w1         ; w1 #>> w2. no need to add, just copy to w3...
  ldx #<w3
  jmp copy2w      ; ...and exit
et3:
  lda tmp2
  cmp #$12
  bcs et2a        ; w1 #>> w2. copy and exit.
  lda #0          ; Summary: w1 is larger than w2, decimal adj in tmp2.
  sta dadj        ; initialize adjustments due to potential overflow.
  jsr w12rab      ; copy w1 and w2 to working regs with decimal adj.
  jsr addrarb     ; perform the addition with overflow adjustment.
  jsr rndrb       ; (optional) round result register.
  lda w1
  sta tmp1        ; 'tmp1' identifies caller to next subroutine.
  jsr rbtow3      ; copy result (rb) to w3.
  rts
adpe:
  jsr diffexp     ; exponent diff: 'a'(hi), 'x'(lo), carry: e1 #>= e2.
  bcs adp1
  jsr xw12
adp1:
  lda tmp1
  cmp #0
  bne et2a        ; e1 #>> e2, just copy w1 to w3 and exit.
  jmp et3
adne:
  jsr diffexp     ; exponent diff: tmp1(hi), tmp2(lo), carry: e1 #>= e2.
  bcc adp1
  jsr xw12
  jmp adp1
.endproc

.proc sumexp
  lda w1   ; return with sum of exponents in tmp1(hi),tmp2(lo)
  and #$0f
  sta tmp1
  lda w2
  and #$0f
  sta tmp2
  clc
  lda w1+1
  bcd_adc w2+1
  tax
  lda tmp1
  bcd_adc tmp2
  sta tmp1
  stx tmp2
  rts
.endproc

.proc diffexp
  lda w1      ; return absolute difference of exponents of w1 and w2.
  and #$0f        ; value is in tmp1(hi),tmp2(lo). carry set if e1 #>= e2,
  sta tmp1        ;  clear if e1 #< e2.
  lda w2
  and #$0f
  sta tmp2
  sec             ; trial difference assuming e1 #>= e2
  lda w1+1        ; (It's just as easy to go ahead and perform the
  bcd_sbc w2+1        ;  subtraction as it is to perform a multibyte
  tax             ;  compare. If the numbers were properly ordered,
  lda tmp1        ;  we are done. Otherwise, we subtract in reverse
  bcd_sbc tmp2        ;  order, which we would have done anyway.)
  bcs :+          ; OK, exit with carry set.
  sec             ; Underflow. e1 is smaller than e2, so recalculate
  lda w2+1        ;  exponent difference.
  bcd_sbc w1+1
  tax
  lda tmp2
  bcd_sbc tmp1
  clc
: sta tmp1
  stx tmp2
  rts
.endproc

;
;   Copy mantissas of numbers in 'w1' and 'w2' to 'ra' and 'rb'.
;   Align decimal points by shifting 'rb' to the right. Decimal
;   adjustment has been previously placed in 'tmp2'.
;
.proc w12rab
  ldy #<wreglen*2-1 ;31
  lda #0
clrlp:           ; clear ra, rb
  sta ra,y
  dey
  bpl clrlp
  clc         ; determine register alignment offset.
  lda tmp2
  bcd_adc #$11    ; this 'magic' number is 2x5+1.
  tay         ;
  lda dec2bin,y ; convert to binary...
  lsr         ; ... then convert to bytes
  tax         ; decimal point adjustment is now contained in 'x'
  ldy #reglen-3
: lda w1+2,y  ; now copy w1 to ra and w2 to rb (mantissas only)
  sta ra+2,y
  lda w2+2,y
  sta rb+2,x
  dex
  dey
  bpl :-
  lda tmp2
  lsr         ; set carry if decimal adjustment is odd...
  bcc cxp1
  ldx #<wreglen-3  ; ...and shift rb left one digit to normalize.
  lda #0
: ldy rb+2,x
  ora tlh,y
  sta rb+2,x
  lda thl,y
  dex
  bpl :-
cxp1:
  rts
.endproc

; copy number from (ptr1) to (ptr2)
; enter with src register page in 'y', src register offset in 'a',
;   dest register offset in 'x' (assumed destination page is for 'wx' regs).
.export copy2w
.proc copy2w
  sty ptr1+1
  sta ptr1
  stx ptr2
  lda #>w1    ; get page used for working registers
  sta ptr2+1
  ldy #7
: lda (ptr1),y
  sta (ptr2),y
  dey
  bpl :-
  rts
.endproc

.proc addrarb
  ldx #reglen-2  ; add previously aligned 'ra' to 'rb', result in 'rb'
  clc
: lda ra+1,x
  bcd_adc rb+1,x
  sta rb+1,x
  dex
  bpl :-
  lda rb+1        ; check for overflow...
  beq :+
  jsr rotrgtb     ; ...and normalize, if needed
  inc dadj        ; and increment decimal adjustment.
: rts
.endproc

.proc subrarb
  ldx #reglen
  sec
: lda ra+1,x
  bcd_sbc rb+1,x
  sta rb+1,x
  dex
  bpl :-
  bcc sabx        ; exit with carry clear (underflow condition).
  lda #12
  sta tmp1
sab1:
  lda rb+2       ; normalize
  and #$f0
  bne sabxaa
  jsr shflftb
  inc dadj        ; this quantity will be subtracted from the exponent.
  dec tmp1
  bne sab1
sabxaa:
  ldy dadj        ; convert dadj to decimal
  lda bin2dec,y
  sta dadj
  sec           ; exit with carry set (normal condition).
sabx:
  rts
.endproc

.proc rndrb
  ldx #7        ; round the result in 'rb'
  lda #$50
  clc
: bcd_adc rb+1,x
  sta rb+1,x
  lda #0
  dex
  bpl :-
  lda rb+1        ; check for overflow...
  beq :+
  jsr rotrgtb     ; ...and normalize, if needed...
  clc
  lda dadj        ; assume dadj #< 99 but (possibly) #> 9
  bcd_adc #1
  sta dadj        ; ... then increment decimal adjustment.
: rts
.endproc

.proc rbtow3
  ldx #5       ; copy rb to w3.
  clc
rbtw1:
  lda rb+2,x      ; first, copy result mantissa from rb to w3.
  sta w3+2,x
  dex
  bpl rbtw1
  lda w3+2
  bne rbta        ; check if zero...
  sta w3+1        ; ...and exit if so.
  sta w3
  rts
rbta:
  lda w1+1        ; now copy exponent from w1 to w3
  sta w3+1
  lda w1
  and #$0f
  sta w3
  bit tmp1        ; adjust exponent, assume dadj #< 99
  bvc rbtw2       ; add adjustment to exponent
  sec
  lda w3+1
  bcd_sbc dadj
  tax
  lda w3
  bcd_sbc #0
  bcs rbtxa
  sec
  lda dadj
  bcd_sbc w3+1
  sta w3+1
  lda #0
  bcd_sbc w3
  sta w3
  lda w1
  and #$c0
  eor #$40
  ora w3
  and #$cf
  sta w3
  rts

rbtw2:
  clc
  lda w1+1
  bcd_adc dadj
  tax
  lda w1
  bcd_adc #0
rbtxa:
  sta w3        ; save exponent
  stx w3+1
  ora w3+1
  bne rbtx
  lda w1          ; if exponent is zero...
  and #$80        ; ...preserve mantissa sign, suppress exponent sign.
  ora w3
  and #$cf
  sta w3
  rts

rbtx:
  lda w1
  and #$c0        ; now copy sign bits to w3.
  ora w3
  and #$cf
  sta w3
  rts
.endproc

.proc xw12
  ldy #7         ; exchange contents of w1 with w2.
: ldx w1,y
  lda w2,y
  sta w1,y
  txa
  sta w2,y
  dey
  bpl :-
  rts
.endproc

.proc shflftb
  ldx #7      ; shift mantissa in rb left one digit.
  lda #0
: ldy rb+1,x
  ora tlh,y
  sta rb+1,x
  lda thl,y
  dex
  bpl :-
  rts
.endproc

.proc rotrgtb
  ldx #7
  lda #0
: ldy rb+1,x
  ora tlh,y
  sta rb+2,x
  lda thl,y
  dex
  bpl :-
  lda #0
  sta rb+1
  rts
.endproc

;
;   multiply routines
;
;   Multiplication is performed by a novel algorithm involving a
;   table of multiples which is pre-computed for each call.
;   Instead of repeatedly adding suitably aligned versions of the
;   multiplier to an accumulated sum, a table consisting of
;   multiples of the multiplicand is accessed by indexing on the
;   current multiplier digit.
;
;   The rationale behind the method is that there are basically
;   two major repetitive operations involved in multiplication.
;   One is the addition of one multi-digit register to another,
;   and the other is a single digit shift of a multi-digit
;   register.
;
;   For 8-bit processors using multi-digit packed BCD numbers
;   (two digits per byte) a two digit shift can be done by
;   simply indexing the bytes by 1 (1 byte equals 2 digits).
;
;   Minimizing the number of BCD adds and shifts is the driving
;   force behind the choice of this algorithm.
;
;   The fixed point table for a specific case looks as follows:
;
;   Multiplicand = 23.456...
;
;       Table
;
;   0   0000000...
;   1   0234560...
;   2   0469120...
;   3   0703680...
;   4   0938240...
;   5   1172800...
;   6   1407360...
;   7   1641920...
;   8   1876480...
;   9   2111049...
;
;   The partial product is updated in two phases. All digits on even
;   boundaries are processed in one pass, and the digits on odd boundaries
;   are processed in the second. This improves the performance of BCD
;   computations by eliminating all but one multi-byte register shift.
;   Note that indexing through memory a byte at a time skips 2 BCD
;   digits at a time.
;
;   Suppose the multiplier is:
;
;               1234567890
;
;   Then the partial products are accumulated during the first pass as if
;   the multiplier were:
;
;               0103050709
;
;   Since the non-zero digits are on byte boundaries, the register
;   alignment can be handled by simply incrementing or decrementing
;   the value in an index register. It is not necessary to shift the
;   multi-digit register during this phase.
;
;   The result register is then shifted left one digit, and the second
;   pass accumulates partial products as if the multiplier were:
;
;               0204060800
;
;
;   The algorithm is summarized as follows:
;
;       1. Create the table of multiples of the multiplicand,
;       2. For each digit of the multiplier on even boundaries:
;           A. get next digit of multiplier,
;           B. using digit as an index, access corresponding table entry,
;           C. add table entry to partial product with proper
;               byte alignment,
;           D. if more digits, goto A.
;       3. Shift multiplier one digit,
;       4. Repeat (2.) for remainder of digits.
;
;   To estimate the performance of this strategy, consider a typical
;   repeated addition algorithm for multi-digit BCD numbers. On the
;   average, there will be 4.5 multi-byte additions per digit of the
;   multiplier.
;   After each digit is processed, the partial product register must
;   be shifted one digit. For 10 digit numbers, there will be
;   approximately 45 adds and 9 register shifts.
;
;   For the method used here, there are 8 adds required to construct
;   the table, and one add per digit of the multiplier. There is only
;   one register shift required, so the total operation count will be
;   18 adds and 1 register shift.
;
.proc mul
  sed
  ldx #47         ; clear working registers
  lda #0
: sta ra,x
  dex
  bpl :-
  jsr mulm        ; get product of mantissas
  lda w1          ; check exponents for same or different signs
  eor w2
  and #$40
  beq ml1s
  jsr diffexp    ; carry set: e1 #>= e2
  bcs :+
  lda w2
  bcc xsgn
: lda w1
  bcs xsgn        ; branch always
ml1s:
  jsr sumexp     ; sum of exponents is in tmp1/tmp2
  lda w1
xsgn:
  and #$40
  sta w3          ; result exponent sign is set
  lda dadj
  beq cxpn        ; branch if no exponent adjustment is needed
  lda w3
  bne xsgna
  jsr incexp
  jmp cxpn
xsgna:
  jsr decexp
  jmp cxpn
cxpn:
  lda tmp2
  sta w3+1
  ora tmp1        ; if exponent is zero, clear exponent sign
  bne cxp2
  sta w3
cxp2:
  lda tmp1
  ora w3
  sta w3
  lda w1          ; now correct mantissa sign
  eor w2
  and #$80
  ora w3
  sta w3
  rts
.endproc

.proc mulm
  lda w1+2       ; multiply mantissas
  beq mzro
  lda w2+2
  beq mzro
  jsr cw12rab
  jsr mktbl       ; create table of multiples...
  jsr psumh       ; ...accumulate partial product...
  jsr shflft      ; ...shift...
  jsr psuml       ; ...now accumulate rest of partial product.
  jsr rc2w3
  rts

mzro:
  ldx #7
: lda #0
  sta w3,x
  dex
  bpl :-
  rts
.endproc

.proc mktbl
  lda #<mtbl     ; create table of multiples of multiplier
  sta mptr
  lda #>mtbl
  sta mptr+1
  sta ptr1+1
  sta ptr2+1
  lda #<mtbl1
  sta ptr1
  lda #<mtbl2
  sta ptr2
  ldx #7
: lda ra+9,x     ; copy multiplier to 1st multiple
  sta mtbl+$10,x
  dex
  bpl :-
  ldx #8
mkolp:
  ldy #7        ; now compute all multiples
  clc
: lda (ptr1),y
  bcd_adc mtbl1,y     ; BCD add
  sta (ptr2),y
  dey
  bpl :-
  cld
  clc             ; bump pointers for next entry
  lda ptr1
  bcd_adc #$10
  sta ptr1
  clc
  lda ptr2
  bcd_adc #$10
  sta ptr2
  sed
  dex
  bne mkolp
  rts
.endproc

.proc cw12rab
  ldx #5
: lda w1+2,x
  sta ra+10,x
  lda w2+2,x
  sta rb+10,x
  dex
  bpl :-
  rts
.endproc

;
;   1st partial sum. add sums of products of high digits from
;   each multiplier byte.
;
.proc psumh
  lda #4          ; set up pointers to working registers
  sta ptr1+1
  sta ptr2+1
  lda #$18
  sta ptr1
  lda #$28
  sta ptr2
  ldy #7
psh1:
  lda (ptr1),y   ; get next high digit
  and #$f0
  beq psh2        ; if zero, no need to add
  sta mptr        ; update pointer to multiple table for this digit
psh1a:
  lda (mptr),y  ; copy this multiple to result register
  sta (ptr2),y
  dey
  bpl psh1a
psh2:
  ldx #6         ; index for number of additional digits in this pass
molp:
  dec ptr1       ; bump pointers down one byte
  dec ptr2
  ldy #7          ; index for register length
  lda (ptr1),y    ; get next digit
  and #$f0
  beq noadd       ; if zero, no need to add
  sta mptr
  clc
: lda (mptr),y   ; add multiple of current digit to partial sum
  bcd_adc (ptr2),y
  sta (ptr2),y
  dey
  bpl :-
noadd:
  dex
  bpl molp        ; next digit
  rts
.endproc

;
;   2nd partial sum. add sums of products with low digits from
;   each multiplier byte.
;
.proc psuml
  lda #$19
  sta ptr1
  lda #$29
  sta ptr2
  ldx #6
psl1:
  dec ptr1
  dec ptr2
  ldy #7
  lda (ptr1),y
  and #$0f
  beq psl3        ; if zero, no need to add
  asl
  asl
  asl
  asl
  sta mptr        ; update pointer to table of multiples for this byte
  clc
psl2:
  lda (mptr),y
  bcd_adc (ptr2),y
  sta (ptr2),y
  dey
  bpl psl2
  bcc psl3
  ldy ptr2
  dey
: lda ra,y
  bcd_adc #0
  sta ra,y
  dey
  bcs :-

psl3:
  dex
  bpl psl1
  rts
.endproc

.proc shflft
  ldx #15      ; shift 'rc' left
  lda #0
: ldy rc,x
  ora tlh,y
  sta rc,x
  lda thl,y
  dex
  bpl :-
  rts
.endproc

; copy/align 'rc' to 'w3'
.proc rc2w3
  lda #1
  sta dadj
  lda rc+3
  and #$f0
  bne rct1
  jsr shflftc
  dec dadj
rct1: ldx #5
: lda rc+3,x
  sta w3+2,x
  dex
  bpl :-
  rts
.endproc

.proc shflftc
  ldx #6      ; shift mantissa in 'rc' left one digit.
  lda #0
: ldy rc+2,x
  ora tlh,y
  sta rc+2,x
  lda thl,y
  dex
  bpl :-
  rts
.endproc

.proc incexp
  clc      ; increment an exponent
  lda tmp2
  bcd_adc #1
  sta tmp2
  lda tmp1
  bcd_adc #0
  sta tmp1
  rts
.endproc

.proc decexp
  sec      ; decrement an exponent
  lda tmp2
  bcd_sbc #1
  sta tmp2
  lda tmp1
  bcd_sbc #0
  sta tmp1
  rts
.endproc

;
;   divide routines
;
;   The divide algorithm uses tables of multiples, as does the
;   multiply algorithm. In this case each multiple is a candidate
;   for subtraction from the current remainder. A quick scan
;   weeds out multiples which are unsuitable for the trial
;   subtraction and a recovery routine is invoked in the event
;   that an underflow occurs anyway.
;
.proc div
  sed
  ;   weed out zero divisor or dividend
  lda w2+2
  bne :+
  jmp dvzrerr
: lda w1+2
  bne d1b
  lda #0
  ldy #7
d0b:
  sta w3,y            ; dividend is zero, return 0.00--e000
  dey
  bpl d0b
  rts
d1b:
  lda #0
  ldx #47
d1:
  sta ra,x            ; clear out working area
  dex
  bpl d1
  ldx #5              ; copy mantissas from w1/w2 to ra/rb
d2:
  lda w2+2,x          ; (ra is divisor, rb is working reg/quotient)
  sta ra+10,x
  lda w1+2,x
  sta rb+2,x
  dex
  bpl d2
  jsr mktbl           ; create table of multiples
  jsr shftlft         ; initialize remainder for 1st digit divide
  lda #15             ; digit counter
  sta ctr2
onedig:
  jsr decdiv       ; divide, one digit at a time
  dec ctr2
  bne onedig
  lda #1          ; copy result to w3 mantissa with dadj = exponent adj.
  sta dadj
  lda rb+8
  beq od1
  jsr shftlft
  dec dadj
  ldx #5
od2:
  lda rb+8,x
  sta w3+2,x
  dex
  bpl od2
  bmi od4         ; branch always
od1:
  ldx #5
od3:
  lda rb+9,x
  sta w3+2,x
  dex
  bpl od3
  ;
  ;   handle exponent
  ;
od4:
  lda w1
  eor w2
  and #$40        ; compare exponent signs
  beq exd
  jsr sumexp
  lda w1
  jmp dexpadj
exd:
  jsr diffexp     ; exponent signs are the same
  lda w2
  bcs dexpadj     ; carry: divisor exp #>= dividend exp (e1 #>= e2)
  eor #$40
dexpadj:
  and #$40
  sta w3
  bit w3
  bvs od5         ; branch to negative exponent handler
  lda dadj        ; positive exponent
  beq od6
  lda tmp2
  ora tmp1
  bne od5a
  lda #1          ; unadjusted exponent is zero and decrement is needed
  sta tmp2
  lda #$40
  ora w3
  sta w3
  jmp od6
od5:
  lda dadj
  beq od6
  jsr incexp
  jmp od6
od5a:
  jsr decexp
od6:
  lda tmp2    ; now copy adjusted exponent
  sta w3+1
  lda tmp1
  ora w3
  sta w3
  and #$0f    ; test for condition of zero exponent...
  ora w3+1    ; ...and clear exponent sign, if so
  bne odx
  sta w3
odx:
  lda w1      ; finally, set +/- mantissa sign.
  eor w2
  and #$80
  ora w3
  sta w3
  rts
.endproc

.proc decdiv
  lda #9       ; compute next quotient digit
  sta ctr1
  lda rb
  ldx rb+1
  cmp mtbl+$90    ; check 9th multiple for valid subtract
  bcc :+
  bne dsub
  cpx mtbl+$91
  bcs dsub
: dec ctr1
  cmp mtbl+$80    ; check 8th multiple
  bcc :+
  bne dsub
  cpx mtbl+$81
  bcs dsub
: dec ctr1
  cmp mtbl+$70    ; check 7th multiple
  bcc :+
  bne dsub
  cpx mtbl+$71
  bcs dsub
: dec ctr1
  cmp mtbl+$60    ; check 6th multiple
  bcc :+
  bne dsub
  cpx mtbl+$61
  bcs dsub
: dec ctr1
  cmp mtbl+$50    ; check 5th multiple
  bcc :+
  bne dsub
  cpx mtbl+$51
  bcs dsub
: dec ctr1
  cmp mtbl+$40    ; check 4th multiple
  bcc :+
  bne dsub
  cpx mtbl+$41
  bcs dsub
: dec ctr1
  cmp mtbl+$30    ; check 3rd multiple
  bcc :+
  bne dsub
  cpx mtbl+$31
  bcs dsub
: dec ctr1
  cmp mtbl+$20    ; check 2nd multiple
  bcc :+
  bne dsub
  cpx mtbl+$21
  bcs dsub
: dec ctr1
  cmp mtbl+$10    ; check 1st multiple
  bcc :+
  bne dsub
  cpx mtbl+$11
  bcs dsub
: dec ctr1        ; must be zero
  lda rb+15
  and #$f0
  jmp shftlft
dsub:
  lda rb+15          ; save current digit in result register
  and #$f0
  ora ctr1
  sta rb+15
  asl
  asl
  asl
  asl
  sta ptr1
  ldy #7
  sec
sulp:
  lda rb,y           ; shift remainder (and quotient) left one digit
  bcd_sbc (ptr1),y
  sta rb,y
  dey
  bpl sulp
  bcs shftlft     ; branch if no correction is needed
  dec rb+15       ; here we recover from underflow caused by
  ldy #7          ;  subtracting a too-large multiple
: lda rb,y       ; correct remainder after underflow
  bcd_adc mtbl+$10,y  ;  by adding 1st multiple back in
  sta rb,y
  dey
  bpl :-
  ; XXX: leak to shftlft
.endproc

.proc shftlft
  ldx #15      ; shift remainder register in rb left one digit.
  lda #0
: ldy rb,x
  ora tlh,y
  sta rb,x
  lda thl,y
  dex
  bpl :-
  rts
.endproc

;
;   square root routines
;
;   (C) 1999, C. Bond. Finds square root of BCD number by
;       non-restoring pseudo-division.
;
.proc sqrt
  sed
  lda w1+2
  bne sq0     ; check for zero
  ldx #7
  lda #0
: sta w3,x   ; square root of zero, clear result and return
  dex
  bpl :-
  rts
sq0:
  bit w1      ; check for negative number...
  bpl sq0b    ; ...and return with domain error.
  jmp domnerr
sq0b:
  ldx #15
  lda #0
: sta ra,x        ; clear working register
  dex
  bpl :-
  ldx #7
: lda w1+2,x
  sta ra+2,x      ; now copy mantissa to ra
  dex
  bpl :-
  lda w1+1        ; check for odd exponent to force grouping of digits
  and #1
  bne sq2
  ldx #7          ; even exponent, shift right one digit
  lda #0
: ldy ra,x
  ora tlh,y
  sta ra+1,x
  lda thl,y
  dex
  bpl :-
sq2:
  ldx #7          ; multiply mantissa by 5
  clc
: lda ra+1,x
  bcd_adc ra+1,x
  sta rb+1,x
  dex
  bpl :-
  ldx #7          ; rb = 2 * ra
  clc
: lda rb+1,x
  bcd_adc rb+1,x
  sta rb+1,x
  dex
  bpl :-
  ldx #7
  clc
: lda ra+1,x
  bcd_adc rb+1,x
  sta ra+1,x
  dex
  bpl :-
  ; ra = 5 * ra
  lda #1
  sta ptr1        ; index used by pseudo-divisor
  lda #$0
  sta rb
  lda #6          ; number of digits counter (x2)
  sta ctr1

sq2d:
  ldx ptr1
  lda #$05
  sta rb,x
  jsr sq3
  dec ctr1
  bmi sqx
  jsr sarlft
  jmp sq2d
sqx:
  ldx #7
  lda #0
: ldy rb+1,x    ;shift rb left one digit
  ora tlh,y
  sta rb+1,x
  lda thl,y
  dex
  bpl :-

  ldx #5
sqx1b:
  lda rb+1,x    ; move mantissa to result register
  sta w3+2,x
  dex
  bpl sqx1b
  lda w1+1        ; calculate exponent (divide by 2)
  sta w3+1
  lda w1
  and #$0f
  sta w3
  lsr w3
  ror w3+1        ; shift exponent right one bit...
  ldy w3
  lda srtbl,y     ; ...and translate to perform BCD correction.
  sta w3
  ldy w3+1
  lda srtbl,y
  sta w3+1
  bit w1          ; is exponent negative?
  bvc sqxx
  lda w1+1        ; if so, adjust the exponent...
  and #$1
  beq sqxc
  clc
  bcd_adc w3+1
  sta w3+1
  lda w3
  bcd_adc #0
  sta w3
sqxc:
  lda #$40       ; ...and correct the result sign
  ora w3
  sta w3
sqxx:
  rts

sq3:
  ldx ptr1
  sec
: lda ra,x
  bcd_sbc rb,x
  sta ra,x
  dex
  bpl :-
  bcc sq4
  ldx ptr1
  clc
  lda rb,x
  bcd_adc #$10
  sta rb,x
  bcc sq3
sq4:
  ldx ptr1
  lda rb,x
  and #$f0
  ora #$9
  sta rb,x
  lda #$50
  sta rb+1,x
  jsr sarlft
  inc ptr1
sq5:
  ldx ptr1
  clc
: lda ra,x
  bcd_adc rb,x
  sta ra,x
  dex
  bpl :-
  bcs sq6
  ldx ptr1
  dec rb-1,x
  bcc sq5
sq6:
  rts
.endproc

.proc sarlft
  ldx #15
  lda #0
: ldy ra,x
  ora tlh,y
  sta ra,x
  lda thl,y
  dex
  bpl :-
  rts
.endproc

;
;   poly routines
;
;   Evaluate a polynomial given a list of polynomial coefficients
;    and argument.
;   On entry, the argument must be in register 'arg' and a
;    pointer to the coefficient list must be in 'pptr'. The
;    number of coefficients is in 'ctr1'.
;
;   Coefficients are in high-to-low order.
;
;   This implementation evaluates a polynomial in the argument
;    squared.
;
.proc poly
  sed
  dec pctr
  ;
  ;   pre-loop initialization
  ;

  ldx #7
pl00:
  lda arg,x      ; get the argument, as given...
  sta w1,x
  sta w2,x
  dex
  bpl pl00
  jsr mul         ; ...and square it for this routine
  ldx #7
pl00a:
  lda w3,x      ; (replace original argument with squared value)
  sta arg,x
  dex
  bpl pl00a
  ldy #7
pl0:
  lda (pptr),y    ; move 1st coefficient to w3
  sta w3,y
  dey
  bpl pl0
  ;
  ;   now enter main loop
  ;
pl1:
  jsr argtow1       ; get argument ...
  jsr w3tow2      ; ... and current result
  jsr mul         ; w3 #<- product
  cld
  clc
  lda pptr        ; bump coefficient pointer to next value
  bcd_adc #8
  sta pptr
  bcc pl1a
  inc pptr+1
pl1a:
  sed
  jsr ctow1      ; get next coefficient...
  jsr w3tow2      ; ... and add to current result
  jsr add
  dec pctr
  bne pl1
  rts
.endproc

.proc ctow1
  ldy #7
cwa:
  lda (pptr),y
  sta w1,y
  dey
  bpl cwa
  rts
.endproc

.proc argtow1
  ldx #7
awa:
  lda arg,x
  sta w1,x
  dex
  bpl awa
  rts
.endproc

.proc w3tow2
  ldx #7
w32a:
  lda w3,x
  sta w2,x
  dex
  bpl w32a
  rts
.endproc

;
;   secant -- calculates secant function: sec(x) = sqrt(1 + tan^2(x))
;
;   w3 = secant(w1)
;
.proc sec_
  jsr tan
  lda #<w3
  ldy #>w3
  ldx #<w4
  jsr copy2w      ; save tangent for (possible) later use
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  jsr mul         ; w3 contains tan^2
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w     ; w3 = 1 + tan^2
  jsr add
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jsr sqrt        ; w3 = sqrt(1 + tan^2), w4 = tan
  lda qdrnt
  cmp #1
  beq :+
  cmp #4
  beq :+
  lda w3
  ora #$80
  sta w3
: rts
.endproc

;
;   sin -- calculates sine function:  sin(x) = tan(x)/sec(x)
;
.proc sin
  jsr sec_
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<w4
  ldy #>w4
  ldx #<w1
  jsr copy2w
  jsr div
  lda w3
  and #$7f
  ldx qdrnt
  cpx #3
  bcc :+
  ora #$80
: sta w3
  rts
.endproc

;
;   cos -- cos(x) = 1/sec(x)
;
.proc cos
  jsr sec_
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
  rts
.endproc

;
;   CORDIC routine to calculate tangent of given angle (radians)
;
;   On entry, 'w1' contains the angle, on exit 'w3' contains the tangent.
;
;   'ra' is used for resolving the angle, where 're' accumulates the
;   pseudo-quotient. After this 'ra', 'rb', 'rc' and 'rd' are the
;   working registers for pseudo-multiplication.
;
;   The computation strategy is as follows:
;
;   1. Scale the given argument.
;      . divide the argument by 2pi,
;      . discard the integer part of the result,
;      . multiply the fractional part by 2pi.
;   2. If the result #< 0 add 2pi so reference angle is positive.
;   3. Find and set quadrant and tnsgn, quadrant is used by sin, cos, etc.
;   4. If arg #> pi subtract pi
;   5. if arg #> pi/2 subtract pi
;   6. The scaled argument has the same tangent as the original argument.
;      It is now in quadrant I or IV.
;      . take the absolute value of the argument.
;   7. The argument is now in the range 0 #<= arg #<= pi/2.
;      . set 'cotflg' = 0
;   8. If the argument exceeds pi/4,
;      . subtract argument from pi
;      . toggle 'cotflg' = 1
;   9. Calculate tangent (see CORDIC description).
;  10. Correct sign of result using 'tnsgn'.
;  11. If 'cotflg' = 1 take reciprocal of result.
;  12. Return F.P. value in w3.
;
;

.proc tan
  lda #0
tan0:
  sta cotflg
  sed
  lda #0
  sta tnsgn
  jsr tanscale    ; reduce argument to range: 0 #<= arg #<= pi/4
  lda #<w3         ; move scaled argument to w1
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda w3+2
  bne :+
  rts
: jsr tanx        ; get tangent
  lda w3
  ora tnsgn       ; correct sign
  sta w3
  lda cotflg
  beq :+
  lda #<unit       ; take reciprocal if cotangent
  ldy #>unit
  ldx #<w1
  jsr copy2w
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  jsr div
: rts
.endproc

.proc cot
  lda #1
  jmp tan::tan0
.endproc

.proc csc
  jsr sin
  lda #<w3
  ldy #> w3
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jmp div
.endproc

.proc tanx                ; main tangent routine
  lda w1+2
  bne tn0
tn0x:
  lda #0
  ldx #7
: sta w3,x        ; argument is zero, clear 'w3' and return.
  dex
  bpl :-
  rts
tn0:
  bit w1
  bvc tn0x1
  lda w1          ; exponent is negative, check size
  and #$0f
  bne tn0x1a      ; large exponent -- return argument
  lda w1+1
  cmp #8
  bcc tn0x1
tn0x1a:
  ldx #7
: lda w1,x        ; large neg exponent -- return argument
  sta w3,x
  dex
  bpl :-
  rts
tn0x1:
  ldx #3*wreglen-1
  lda #0          ; clear working registers
tn0xb:
  sta ra,x
  dex
  bpl tn0xb
  lda #<kxatn      ; initialize arctangent table pointer
  sta ptr1
  lda #>kxatn
  sta ptr1+1
  ;
  lda w1+1
  lsr
  cld
  bcd_adc #5
  sed
  tax
  ldy #5
tn00b:
  lda w1+2,y    ; move angle to 'ra' with decimal point alignment
  sta ra+2,x
  dex
  dey
  bpl tn00b
  lda w1+1
  and #1
  beq tn00a
  ldx #12
  lda #0
tn00c:
  ldy ra+2,x
  ora tlh,y
  sta ra+2,x
  lda thl,y
  dex
  bpl tn00c
tn00a:
  jsr tnpdiv      ; call tangent pseudo-divide routine
  ;
  ;   're' contains pseudo-quotient in extended format.
  ;       (00 0D 0D 0D 0D 0D ...)
  ;
  ;   Note that the first digit is zero because the argument
  ;   scaling reduces the argument below pi/4 #< 1.0.
  ;   now call routine to create fixed point tangent
  ;
  jsr tnpmul

  ;   On return, 'ra' contains a quantity proportional to 'y', and 'rb'
  ;   contains a quantity proportional to 'x'. First, convert the quantities
  ;   from the fixed point form to floating point in 'w1' and 'w3'.
  ;
  lda #0      ; clear exponent and sign field of 'w1'
  sta w1
  sta w1+1
  lda #8
  sta bctr    ; limit iterations
t0a1:
  lda ra+2
  and #$f0    ; is the decimal point OK?
  bne t0b
  lda #$40
  sta w1      ; exponent is negative
  inc w1+1
  lda #0
  ldx #12
t0a2:
  ldy ra+2,x ; shift 'ra' left and adjust exponent in 'w1'
  ora tlh,y
  sta ra+2,x
  lda thl,y
  dex
  bpl t0a2
  dec bctr
  bne t0a1
  lda #0
  ldx #7
t0a3:
  sta w3,x       ; clear 'w3' to zero
  dex
  bpl t0a3
  rts
t0b:
  ldx #5
t0b1:
  lda ra+2,x     ; move mantissa to 'w1'
  sta w1+2,x
  dex
  bpl t0b1
  ;
  ;   check 'rb' position
  ;
  lda rb+1
  beq t0b2
  ldx #7
t0b1a:
  lda w1,x      ; 'w1' contains result
  sta w3,x
  dex
  bpl t0b1a
  rts
t0b2:
  lda #8
  sta bctr
  lda #$40
  sta w2
  lda #0
  sta w2+1        ; 'rb' has negative exponent
t0b2a:
  lda rb+2      ; shift left until aligned
  and #$f0
  bne t0c
  inc w2+1
  lda #0
  ldx #10
t0b3:
  ldy rb+2,x
  ora tlh,y
  sta rb+2,x
  lda thl,y
  dex
  bpl t0b3
  dec bctr
  bne t0b2a
  ldx #7
t0b4:
  lda pio2,x     ; angle is pi/2  ('rb' is zero)
  sta w3,x
  dex
  bpl t0b4
  rts
t0c:
  ldx #5
t0c1:
  lda rb+2,x     ; move mantissa to 'w2'
  sta w2+2,x
  dex
  bpl t0c1
  jsr div
  rts
  ;
  ; tangent pseudo-multiply routine
  ; enter with 'ra' = residual = y
  ; set 'rb' = 1.0             = x
  ; copy 'ra' and 'rb' to 'rc' and 'rd'
  ;
tnpmul:
  ldx #$10
  stx rb+2
  ldx #10
: lda ra+2,x
  sta rc+2,x
  lda rb+2,x
  sta rd+2,x
  dex
  bpl :-
  lda #0
  sta ctr3    ; pseudo-digit index
  ;
  ; repeat algorithm for each digit of pseudo-quotient
  ;
  ;   ra = y2
  ;   rb = x2
  ;   rc = y1
  ;   rd = x1
  ;
tnpm1:
  ldx ctr3
  dec re,x
  bmi tnpm2
  sed
  ;
  ;   copy 'ra', 'rb' to 'rc','rd'
  ;
  ldx #10
: lda ra+2,x
  sta rc+2,x
  lda rb+2,x
  sta rd+2,x
  dex
  bpl :-

  ;   now shift 'rc' and 'rd' right
  ;
  lda ctr3
  jsr scdnrt
  ;
  ;   now perform pseudo-multiplication
  ;
  ldx #10
  sec
: lda rb+2,x      ; x2 = x1 - y1*10^j
  bcd_sbc rc+2,x
  sta rb+2,x
  dex
  bpl :-
  ldx #10
  clc
: lda ra+2,x      ; y2 = y1 + x1*10^j
  bcd_adc rd+2,x
  sta ra+2,x
  dex
  bpl :-
  jmp tnpm1
tnpm2:
  inc ctr3
  lda #5
  cmp ctr3
  bcs tnpm1
tnpmx:
  rts
.endproc

;
;   tangent pseudo-divide routine: calculates CORDIC pseudo-quotient
;   for tangent function.
;
.proc tnpdiv
  lda #5       ; number of table entries in arctangent table
  sta ctr3
  ldx #0          ; index into result register (rd)
  stx re
tpd0:
  ldy #7         ; (subtract until underflow, then restore)
  sec
tpd1:
  lda ra+2,y     ; subtract 1 instance of current table value
  bcd_sbc (ptr1),y
  sta ra+2,y
  dey
  bpl tpd1
  bcc tpd2a       ; underflow? no, do another loop
  inc re,x
  jmp tpd0
tpd2a:
  ldy #7          ; restore
  clc
tpd2:
  lda ra+2,y
  bcd_adc (ptr1),y
  sta ra+2,y
  dey
  bpl tpd2
  inx
  lda #0
  sta re,x
  cld             ; ...bump tangent table pointer to next entry
  clc
  lda ptr1
  bcd_adc #8
  sta ptr1
  bcc tpd3
  inc ptr1+1
tpd3:
  sed
  dec ctr3
  bpl tpd0        ; loop until each digit of pseudo-quotient is done
  rts
.endproc

;
;   x2z: transfer 'x' to 'z' with existing offsets
;
.proc x2z
  ldx #9
  lda #0
x2z0:
  sta rd,x       ; clear 'z'
  dex
  bpl x2z0
  ldy #7
x2z1:
  lda (ptr2),y
  sta (ptr3),y
  dey
  bpl x2z1
  rts
.endproc

;
;   arccos -- calculates arcosine(x): acos(x) = pi/2 - asin(x)
;
.proc acos
  jsr asin
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<pio2
  ldy #>pio2
  ldx #<w1
  jsr copy2w
  jsr sub
  rts
.endproc

;                                  x
;   arcsine --  asin(x) = atan( ---------)
;                               ы 1 - x^2
.proc asin
  lda w1+2
  bne :+
  jmp retzr
:
  lda w1
  and #$80        ; save argument sign (append to result)
  sta sinflg
  lda #<w1          ; make argument positive
  and #$7f
  sta w1
  lda #<w1        ; save to temporary register
  sta ptr1
  lda #>w1
  sta ptr1+1
  lda #<reg1
  sta ptr2
  lda #>reg1
  sta ptr2+1
  jsr copyreg
  lda #<w1
  ldy #>w1
  ldx #<w2
  jsr copy2w
  jsr mul         ; form arg^2
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  jsr sub         ; form 1 - arg^2
  ;
  ;   test result of 1 - arg^2
  ;
  ;   if zero, argument is +-1.0 and angle is +-pi/2 (sin) or -pi (cos)
  ;   if negative, argument is invalid.
  ;
  lda w3+2
  beq argzr
  lda w3
  bpl argpls
  jmp rangerr     ; argument #> 1.0, return with error
argzr:
  lda #<pio2
  ldy #>pio2
  ldx #<w3
  jsr copy2w
  jmp fxsgn
argpls:
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jsr sqrt        ; form ы 1 - arg^2
  lda #<reg1
  ldy #>reg1
  ldx #<w1
  jsr copy2w
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  jsr div         ; form arg / ы 1 - arg^2
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jsr atan

fxsgn:
  lda w3
  ora sinflg
  sta w3
  rts
.endproc

;
;   arctangent
;
;   1. Test argument sign,
;       . save sign, make argument positive.
;   2. Test size of argument,
;       . if argument #< 1e-6 correct sign and return
;         argument in w3.
;       . if argument #> 1.0, set cotflg replace argument
;         with its reciprocal.
;       . else clear cotflg.
;   3. Compute arctangent of argument. (0 #<= angle #<= pi/4).
;   4. If cotflg, subtract from pi/2.
;   5. Correct sign.
;   6. Return result in w3.
;
.proc atan
  sed
  lda #0
  sta cotflg
  sta tnsgn
  bit w1
  bpl :+      ; is argument negative ?
  lda #$80    ; yes, set 'tnsgn' flag
  sta tnsgn   ; and make arg +
  lda #$4f
  and w1
  sta w1
: bvs argchk
  jsr inv
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  inc cotflg  ; argument is inverted and cotflg is set
argchk:          ; here, 0 #<= arg #<= 1.0
  lda w1
  and #$0f
  bne toosml
  lda w1+1
  cmp #7
  bcc argok
toosml:
  lda #0   ; argument is too small to need computation
  cmp cotflg
  beq tsx_
  lda #<pio2
  ldy #>pio2
  ldx #<w2
  jsr copy2w
  jsr div
tsx_:
  lda tnsgn
  ora w1
  sta w1
  lda #<w1
  ldy #>w1
  ldx #<w3
  jsr copy2w
  rts
argok:
  ldx #5*wreglen-1
  lda #0
atn00a:
  sta ra,x         ; clear working storage: ra, rb, rc, rd and re
  dex
  bpl atn00a
  ;
  ;   move argument to ra with decimal point alignment
  ;
  lda w1+1
  lsr
  cld
  clc
  bcd_adc #5
  sed
  tax
  ldy #5
: lda w1+2,y
  sta ra+2,x
  dex
  dey
  bpl :-

  lda w1+1
  and #1
  beq :+
  ldx #7          ; even exponent, shift right one digit
  lda #0
atn1:
  ldy ra+2,x
  ora tlh,y
  sta ra+3,x
  lda thl,y
  dex
  bpl atn1
  sta ra+2
: ldx #0
  stx ctr3        ; storage for pseudo-quotient index
  lda #$10        ; 'ra' and 'rb' are now initialized
  sta rb+2
  ;
  ;   ra = y2 = arg (0 #< arg #< pi/4)
  ;   rb = x2 = 1.0
  ;
  ;   1. initialize pseudo-quotient digit counter and index
  ;       ctr3 #<- index = 0, re used for pq
  ;   2. outer loop
  ;      (A) inner loop
  ;        y2 -> y1   (rc)
  ;        x2 -> x1   (rd)
  ;        y2 shr index (10^j)  (1st pass, j=0, no shift required)
  ;        x2 shr index (10^j)    "   "     "        "      "
  ;        y2 = y2 - x1
  ;        if y2 #< 0 goto (B)
  ;        x2 = x2 + y1
  ;        re[index]++
  ;        goto (A)
  ;     (B) y2 = y2 + x1  (restore after subtraction underflow)
  ;        index++
  ;        if index is LT 7 goto 2.
  ;
  lda #0
  sta ctr3
atnlp1:
  ldx #6
: lda ra+2,x    ; y2,x2 -> y1,x1
  sta rc+2,x
  lda rb+2,x
  sta rd+2,x
  dex
  bpl :-
  lda ctr3
  beq :+
  jsr scdnrt  ; shift y1,x1 to correct postion
: sed
  sec
  ldx #6
: lda ra+2,x
  bcd_sbc rd+2,x
  sta ra+2,x
  dex
  bpl :-
  bcc rstr        ; underflow: do restore
  clc
  ldx #6
: lda rb+2,x
  bcd_adc rc+2,x
  sta rb+2,x
  dex
  bpl :-
  ldx ctr3
  inc re,x        ; update current pseudo-quotient digit
  jmp atnlp1
rstr:
  ldx #6
  clc
: lda ra+2,x
  bcd_adc rd+2,x
  sta ra+2,x
  dex
  bpl :-
  inc ctr3
  lda #6          ; number of desired pseudo-quotient digits
  cmp ctr3
  bcs atnlp1
  ;
  ;   pseudo-quotient digits are now in 're' (6 digits)
  ;   1. Convert 'ra' to F.P. in w1
  ;   2. Convert 'rb' to F.P. in w2
  ;   3. jsr divide, result in w3 is residual starting value for next step
  ;   4. move w3 to 'ra' with decimal point alignment
  ;   5. pseudo-multiply. set 'i' to 0.
  ;       A. for each digit[i] in pseudo-quotient, add kxatnx[i]
  ;       B. decrement digit
  ;       C. digit = 0?
  ;           no? goto A.
  ;           yes? increment 'i'
  ;       D. convert 'ra' to F.P. in w3, return
  ;
  lda #10
  sta bctr        ; guard counter
  lda #0
  sta w1+1
  lda #$40
  sta w1
  lda #0
  sta w1+1        ; 'rs' has negative exponent
: lda ra+2         ; shift left until aligned
  and #$f0
  bne atnres
  inc w1+1
  lda #0
  ldx #10
: ldy ra+2,x
  ora tlh,y
  sta ra+2,x
  lda thl,y
  dex
  bpl :-
  dec bctr
  bne :-- ; XXX: was @B1, check what it means
atnres:
  ldx #5
: lda ra+2,x
  sta w1+2,x
  dex
  bpl :-
  ldx #7
: lda rb,x
  sta w2,x
  dex
  bpl :-
  jsr div
  ;
  ;   pseudo-multiply for arctangent
  ;
  ;   1. copy w3 to 'ra' with decimal point aligned
  ;   2. initialize kxatn table pointer in 'ptr1'
  ;   3. initialize pseudo-quotient index in 'ctr3'
  ;   4. pseudo-multiply by repeated addition:
  ;       A. for count given by each digit of pseudo-quotient,
  ;           add current table entry to running sum in 'ra',
  ;       B. increment index (ctr3) and bump table pointer
  ;           to next entry.
  ;       C. if index #< 7, goto A.
  ;
  ldx #15
  lda #0
: sta ra,x    ; clear 'ra'
  dex
  bpl :-
  lda w3
  and #$0f
  bne atnpm
  lda w3+1
  cmp #9
  bcs atnpm
  lsr
  clc
  cld
  bcd_adc #5
  tax
  ldy #5
: lda w3+2,y
  sta ra+2,x
  dex
  dey
  bpl :-
  lda #<w1+1
  and #1
  beq atnpm
  ldx #7          ; shift right one digit
  lda #0
: ldy ra+2,x
  ora tlh,y
  sta ra+3,x
  lda thl,y
  dex
  bpl :-
  sta ra+2
atnpm:
  lda #<kxatn ; setup table point
  sta ptr1
  lda #>kxatn
  sta ptr1+1
  ldx #0      ; initialize pseudo-quotient index
  stx ctr3
apmlp:
  dec re,x
  bmi atnxtd
  ldy #7
  sed
  clc
: lda ra+2,y
  bcd_adc (ptr1),y
  sta ra+2,y
  dey
  bpl :-
  bmi apmlp
atnxtd:
  inc ctr3
  cld
  clc
  lda ptr1
  bcd_adc #8
  sta ptr1
  bcc :+
  inc ptr1+1
: ldx ctr3
  cpx #7      ; number of pseudo-quotient digits
  bcc apmlp
  lda #0
  sta w3
  sta w3+1        ; clear result exponent field
atn02a:
  lda ra+2
  and #$f0
  bne atn03
  lda #$40        ; negative exponent
  sta w3
  ldx #15
  lda #0
atn02b:
  ldy ra+2,x   ;shift 'ra' left until normalized
  ora tlh,y
  sta ra+2,x
  lda thl,y
  dex
  bpl atn02b
  inc w3+1
  jmp atn02a
atn03:
  ldx #5
: lda ra+2,x
  sta w3+2,x
  dex
  bpl :-
  lda cotflg
  beq  atnx
  lda #<pio2
  ldy #>pio2
  ldx #<w1
  jsr copy2w
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  jsr sub
atnx:
  lda w3
  ora tnsgn
  sta w3
  rts
.endproc

;
;   arc cosecant -- acsc(x) = asin(1/x)
;
;   w3 = acsc(w1)
;
.proc acsc
  lda #<w1
  ldy #>w1
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
  jmp asin
.endproc

;
;   arc secant -- asec(x) = acos(1/x)
;
;   w3 = asec(w1)
;
.proc asec
  lda #<w1
  ldy #>w1
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
  jmp acos
.endproc

;
;   arc cotangent -- acot(x) = atan(1/x)
;
;   w3 = acot(w1)
;
.proc acot
  lda #<w1
  ldy #>w1
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
  jmp atan
.endproc

;
;   loge
;
;   calculate the natural logarithm (ln) of the argument in 'w1'.
;
;   w3 = ln(w1)
;
.proc loge
  lda w1
  bpl :+          ; test for negative number
  jmp domnerr
: lda w1+2
  bne :+          ; test for zero argument
  jmp rangerr
: ldx #7
: lda w1,x        ; save argument
  sta reg1,x
  dex
  bpl :-
  lda w1          ; if argument is near unity, use series approximation
  bne :+          ;  9.9 e-1 #< arg #< 1.01 e0
  lda w1+1
  bne :+
  lda w1+2
  cmp #$10
  bne :+
  ;
  ;   argument is near unity. test for series solution
  ;
  lda #<unit
  ldy #>unit
  ldx #<w2
  jsr copy2w
  jsr sub
  lda w3+2
  bne :+          ; check for arg = 0.0
  jmp retzr
: lda w3
  cmp #$40
  bne lnargrst
  lda w3+1
  cmp #$3
  bcc lnargrst
  jmp lnser
lnargrst:
  lda #<reg1
  ldy #>reg1
  ldx #<w1
  jsr copy2w
  jsr clr_r       ; clear all workspace regs
  ldx #5          ; copy mantissa to 'ra'
: lda w1+2,x
  sta ra+2,x
  dex
  bpl :-
  lda #0
  sta ctr1        ; 'j' index for pseudo-divide
  lda #6
  sta ctr3
  ;
  ;   main loop
  ;
lnlp0:
  jsr cpyra2rc      ; compute each pseudo-quotient digit
  lda ctr1
  jsr shfrcn
  jsr addrc2ra
  bcs :+
  ldx ctr1
  inc rd,x
  jmp lnlp0

: jsr subrc2ra        ; restore remainder after underflow
  inc ctr1
  dec ctr3
  bne lnlp0           ; another iteration?

  ldx #6
  sec
: lda #0              ; compute residual to start pseudo-multiply
  bcd_sbc ra+2,x
  sta ra+2,x
  dex
  bpl :-

  ldx #7          ; shift right one digit
  lda #0
: ldy ra+2,x
  ora tlh,y
  sta ra+3,x
  lda thl,y
  dex
  bpl :-
  sta ra+2
  ;
  ;   pseudo-multiply
  ;
  lda #<kxlog     ; set pointer to table of logs
  sta ptr1
  lda #>kxlog
  sta ptr1+1
  lda #0          ; initialize counters
  sta ctr1
  lda #6
  sta ctr2
lnlp1:
  ldx ctr1      ; perform pseudo-multiply for each table entry
  dec rd,x
  bmi npsm
  ldy #6
  clc
: lda ra+2,y
  bcd_adc (ptr1),y
  sta ra+2,y
  dey
  bpl :-
  bmi lnlp1
npsm:
  cld            ; bump table pointer to next coefficient
  clc
  lda ptr1
  bcd_adc #8
  sta ptr1
  bcc :+
  inc ptr1+1
: sed
  inc ctr1
  dec ctr2
  bne lnlp1
  ;
  ;   'ra' contains ln(mantissa). adjust exponent if needed
  ;
: lda ra+2        ; (ctr2 is now 0)
  and #$f0
  bne ln2fp
  jsr shflfta
  inc ctr2
  bne :-
ln2fp:
  ldx #5        ; save current logarithm as F.P. in reg2
: lda ra+2,x
  sta reg2+2,x
  dex
  bpl :-
  lda #$80        ; sign of mantissa?
  sta reg2
  lda ctr2
  sta reg2+1
  beq :+
  lda #$40        ; sign of exponent
  ora reg2
  sta reg2
:
  jsr exp2fp
  lda #<unit
  ldy #>unit
  ldx #<w2
  jsr copy2w
  jsr add
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<ln10
  ldy #>ln10
  ldx #<w2
  jsr copy2w
  jsr mul
  ldx #7
: lda reg2,x
  sta w1,x
  dex
  bpl :-
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  jsr add
  rts
.endproc

;
;   exp2fp -- convert exponent in 'w1' to signed F.P. number in 'w1'
;
.proc exp2fp
  ldx #5
  lda #0
: sta w1+2,x  ; clear mantissa space
  dex
  bpl :-
  lda w1
  and #$7f
  sta w1
  and #$40
  asl
  tax         ; save sign bit
  lda w1
  and #$f
  bne bigexp
  stx w1
  lda w1+1
  and #$f0
  bne medexp
  lda w1+1
  asl
  asl
  asl
  asl
  sta w1+2
  lda #0
  sta w1+1
  stx w1
  rts
medexp:
  sta w1+2
  lda #1
  sta w1+1
  stx w1
  rts

bigexp:
  asl
  asl
  asl
  asl
  sta w1+2
  lda w1+1
  lsr
  lsr
  lsr
  lsr
  ora w1+2
  sta w1+2
  lda w1+1
  asl
  asl
  asl
  asl
  sta w1+3
  lda #2
  sta w1+1
  stx w1
  rts
.endproc

.proc cpyra2rc
  ldy #6
  lda ra+2,y
  sta rc+2,y
  dey
  bpl :-
  rts
.endproc

;
;   add 'rc' to 'ra'. 'rc' is shifted right by ctr1 digits
;
.proc addrc2ra
  cld
  clc
  ldx #7
  sed
: lda ra+2,x
  bcd_adc rc+2,x
  sta ra+2,x
  dex
  bpl :-
  rts
.endproc

;
;   subtract 'rc' from 'ra'. 'rc' is shifted right by ctr1 digits
;
.proc subrc2ra
  cld
  sec
  sed
  ldx #7
: lda ra+2,x
  bcd_sbc rc+2,x
  sta ra+2,x
  dex
  bpl :-
  rts
.endproc

;
;   shift 'ra' left one digit
;
.proc shflfta
  ldx #8
  lda #0
: ldy ra+2,x
  ora tlh,y
  sta ra+2,x
  lda thl,y
  dex
  bpl :-
  rts
.endproc

.proc shfrcn
  sta ctr2 ; shift rc right n digits ('n' in a_reg)
  and #1      ; is it an odd number?
  beq src0    ; no, only do byte shifts
  lda #0      ; yes, do single digit shift
  ldx #7
: ldy rc+2,x
  ora tlh,y
  sta rc+3,x
  lda thl,y
  dex
  bpl :-
  sta rc+2
src0:
  lda ctr2
  lsr         ; convert to bytes
  beq srcx    ; exit if no further shift required
  cld
  clc
  bcd_adc #7
  sed
  tay
  ldx #7
: lda rc+2,x
  sta rc+2,y
  dey
  dex
  bpl :-
  lda #0
: sta rc+2,y
  dey
  bpl :-
srcx:
  rts
.endproc

;
;   lnser -- return logarithm of number near unity by series expansion
;            enter with (arg - 1) in 'w3'.
;
.proc lnser
  lda #<kln
  sta ptr3
  lda #>kln
  sta ptr3+1
  lda #3
  sta ctr1
  ldx #7
: lda w3,x    ; save argument (x) to 'reg2'
  sta reg2,x
  dex
  bpl :-
  ldy #7
: lda (ptr3),y    ; preload 'w3' with 1st coefficient
  sta w3,y
  dey
  bpl :-
  ; main loop
lnslp:
  ldy #7
: lda w3,y
  sta w1,y
  lda reg2,y
  sta w2,y
  dey
  bpl :-
  jsr mul     ; multiply
  dec ctr1
  bmi lnsrx
  cld
  clc
  lda ptr3
  bcd_adc #8
  sta ptr3
  bcc :+
  inc ptr3+1
: sed
  ldy #7
: lda (ptr3),y
  sta w1,y
  lda w3,y
  sta w2,y
  dey
  bpl :-
  jsr add     ; add next coefficient
  jmp lnslp
lnsrx:
  rts
.endproc

;
;   exp
;
;   calculate exp(arg) where the argument is in 'w1'.
;
;   First, the argument is checked for sign. If the sign of the
;   argument is negative, the absolute value of the argument
;   is sent to the 'exp' routine and the reciprocal of the result
;   is returned to the caller.
;
;   Next, the integer part of the argument is taken and used to
;   calculate a power-of-ten multiplier (number of decimal places)
;   is found. A residual decimal fraction from this operation is
;   added to the fractional value of the given argument and the
;   'exp' routine is called with this sum as its argument.
;
;   The result is combined with the previously calculated decimal
;   point adjustment and a test for taking the reciprocal is made.
;   After all operations are complete, the result is returned to
;   the caller.
;
;   This implementation takes the mantissa and treats it as if
;   it were in the range: 0.1 #<= m #<= 0.99999.... instead of
;   1.0 #<= m #<= 9.99999......
;
;   The algorithm works by finding a pseudo-quotient from the
;   mantissa using a table of natural logs of 2, 1.1, 1.01, 1.001,
;   1.0001, etc. The digits of the pseudo-quotient represent the
;   number of subtractions of the appropriate log before underflow.
;
;   After 6-8 pseudo-quotient digits have been found, 1.0 is added
;   to the residual and the pseudo-multiply algorithm is run.
;
;   q = arg/ln10
;   f = floor(q)
;   r = q - f
;
;
.proc exp
  lda #0
  sta expflg
  lda w1
  and #$0f
  beq :+
xpre:
  jmp rangerr
: lda w1+2
  bne :+
xrt1:
  lda #<unit
  ldy #>unit
  ldx #<w3
  jmp copy2w
: bit w1
  bpl :+
  lda #$80
  sta expflg
  eor w1
  sta w1
: bit w1
  bvc :+
  lda w1      ; check for range problem with exponent
  and #$0f
  bne xrt1    ; large negative exponent returns 1.0000...
  lda w1+1
  cmp #$12
  bcs xrt1
  bcc :++ ; XXX: was @F1, check what it means
: lda w1+1
  cmp #4
  bcs xpre
  cmp #3
  bne :+
  lda w1+2
  cmp #$23
  bcs xpre
: lda #<iln10      ; form arg/ln10
  ldy #>iln10
  ldx #<w2
  jsr copy2w
  jsr mul
  lda #<w3
  ldy #>w3
  ldx #<reg1
  jsr copy2w      ; save backup copy of arg/ln10 in 'reg1'
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jsr int
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<w3
  ldy #>w3
  ldx #<reg2       ; save power of 10 (exponent) in 'reg2'
  jsr copy2w
  jsr sub
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<ln10
  ldy #>ln10
  ldx #<w2
  jsr copy2w
  jsr mul
  lda #0
  sta tmp1        ; clear shift count register
  bit w3
  bvc xshft
  lda w3+1
  cmp #$12
  bcc :+
  jmp xrt1
: tax
  lda dec2bin,x
  sta tmp1
xshft:
  jsr xexp
  ;
  ;   mantissa of exponential is in 'w1'. get exponent from 'reg2'
  ;
  ldy reg2+1
  lda #0
  sta reg2+1
xxp:
  ldx #3       ; move exponent value from mantissa to exponent
: asl reg2+3
  rol reg2+2
  rol reg2+1
  rol reg2
  dex
  bpl :-
  dey
  bpl xxp
  lda reg2     ; copy exponent to w1
  sta w3
  lda reg2+1
  sta w3+1
  bit expflg
  bpl :+
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
:
  rts
.endproc

;
;   xexp -- main exponential computation routine
;
.proc xexp
  jsr clr_r
  ldx #5
  lda tmp1
  lsr         ; ignore odd bit for now
  cld
  clc
  bcd_adc #5
  tay
: lda w3+2,x
  sta ra+2,y
  sta rb+2,x
  dey
  dex
  bpl :-
  lda tmp1
  sta re
  lsr         ; do odd bit shift (if any)
  bcc :++

  ldx #8
  lda #0
: ldy ra+1,x   ;   12 34 00
  ora tlh,y    ;   01 23 40
  sta ra+2,x
  lda thl,y
  dex
  bpl :-
  sta ra+2,x
:
  ;
  ;   this is the routine which computes the natural logarithm of
  ;   the number in 'ra'.
  ;
xnl:
  lda #<kxlog
  ldy #>kxlog
  sta ptr1
  sty ptr1+1
  lda #7
  sta tmp2        ; 'tmp2' is outer loop counter
  lda #0
  sta ctr1
  ;
  ;   subtract 'klog' entry from 'ra' until underflow
  ;
mxlp0:
  lda #0
  tax
mxlp:
  sed
  sec
  ldy #7
: lda ra+2,y
  bcd_sbc (ptr1),y
  sta ra+2,y
  sta rc+2,y
  dey
  bpl :-
  inx
  bcs mxlp
  dex
  txa              ; 'x' has current pseudo-quotient digit
  ldx ctr1
  sta rd,x
  inc ctr1
  clc
  ldy #7
: lda ra+2,y         ; restore 'ra' after underflow
  bcd_adc (ptr1),y
  sta ra+2,y
  dey
  bpl :-
  cld
  clc
  lda ptr1
  bcd_adc #8
  sta ptr1
  bcc :+
  inc ptr1+1
: dec tmp2
  bpl mxlp0
  ;
  ;   pseudo-quotients are found, now perform pseudo-multiply
  ;
  lda #0      ; 'ctr1' is offset to pseudo-quotient digit
  sta ctr1
  lda #$10    ; add '1' to residual in 'ra'
  sta ra+2
  ldx #7
  stx ctr3    ; 'ctr3' is outer loop counter (number of digits)
  ldx #7
: lda ra+2,x  ; copy 'ra' to 'rc'
  sta rc+2,x
  dex
  bpl :-
  ;
  ;  multiply 'ra' by 2.0, 1.1, 1.01, etc. iterate each multiplier
  ;   as controlled by pseudo-digit in 'rd'
  ;
xml:
  ldx ctr1
  dec rd,x
  bmi xbd
  ;
  ;   copy 'ra' to 'rc' shifted right
  ;
  ldx #7
: lda ra,x
  sta rc,x
  dex
  bpl :-
  lda ctr1
  jsr shfrcn
  sed
  ldx #5
  clc
: lda ra+2,x
  bcd_adc rc+2,x
  sta ra+2,x
  dex
  bpl :-
  jmp xml
xbd:
  inc ctr1
  dec ctr3
  bpl xml
  ldx #5
: lda ra+2,x
  sta w3+2,x
  dex
  bpl :-
  lda #0
  sta w3
  sta w3+1
  rts
.endproc

;
;   hyberbolic sine -- calculates sinh(x) = (exp(x)-exp(-x))/2
;
;   w3 = sinh(w1)
;
.proc sinh
  jsr exp
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda w1
  ora #$80
  sta w1
  jsr add
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<half
  ldy #>half
  ldx #<w2
  jsr copy2w
  jmp mul
.endproc

;
;   hyperbolic cosine -- calculates cosh(x) = (exp(x) + exp(-x))/2
;
;   w3 = cosh(w1)
;
.proc cosh
  jsr exp
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jsr add
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<half
  ldy #>half
  ldx #<w2
  jsr copy2w
  jmp mul
.endproc

;
;   tanh -- compute (exp(x) - exp(-x))/(exp(x) + exp(-x))
;
;   w3 = tanh(w1)
;
.proc tanh
  jsr exp
  lda #<w3         ; w3 = exp(x)
  ldy #>w3
  ldx #<w2
  jsr copy2w      ; w2 = exp(x)
  ldx #7
: lda w3,x        ; save exp(x) in reg2
  sta reg2,x
  dex
  bpl :-

  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div         ; w3 = exp(-x)
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w      ; w2 = exp(-x)
  ldx #7
: lda w3,x        ; save exp(-x) in reg3
  sta reg3,x
  dex
  bpl :-
  lda #<reg2
  ldy #>reg2
  ldx #<w1
  jsr copy2w
  jsr sub         ; w3 = exp(x) - exp(-x)
  ldx #7
: lda reg2,x      ; w1 = exp(x)
  sta w1,x
  dex
  bpl :-
  ldx #7
: lda reg3,x      ; w2 = exp(-x)
  sta w2,x
  dex
  bpl :-
  ldx #7
: lda w3,x        ; reg2 = exp(x) - exp(-x)
  sta reg2,x
  dex
  bpl :-
  jsr add         ; w3 = exp(x) + exp(-x)
  ldx #7
: lda w3,x        ; w1 = exp(x) + exp(-x)
  sta w2,x
  dex
  bpl :-
  ldx #7
: lda reg2,x      ; w2 = exp(x) - exp(-x)
  sta w1,x
  dex
  bpl :-
  jmp div
.endproc

;
;   hyperbolic cosecant -- csch(x) = 1/sinh(x)
;
;   w3 = csch(w1)
;
.proc csch
  jsr sinh
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jmp div
.endproc

;
;   hyperbolic secant -- sech(x) = 1/cosh(x)
;
;   w3 = cosh(w1)
;
.proc sech
  jsr cosh
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jmp div
.endproc

;
;   hyperbolic cotangent -- coth(x) = 1/tanh(x)
;
;   w3 = coth(w1)
;
.proc coth
  jsr tanh
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jmp div
.endproc

;
;   hyperbolic arc sine -- asinh(x) = ln(x + sqrt(x^2 + 1))
;
;   w3 = asinh(w1)
;
.proc asinh
  ldx #7
: lda w1,x        ; save argument 'x' in reg1
  sta reg1,x
  dex
  bpl :-
  lda #<w1
  ldy #>w1
  ldx #<w2
  jsr copy2w
  jsr mul          ; form x^2
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr add         ; form 1 + x^2
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jsr sqrt        ; w3 = sqrt(1 + x^2)
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<reg1
  ldy #>reg1
  ldx #<w1
  jsr copy2w
  jsr add         ; w3 = x + sqrt(1 + x^2)
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jmp loge
.endproc

;
;   hypberbolic arc cosine: acosh(x) = ln(x + sqrt(x^2 - 1))
;
.proc acosh
  ldx #7
: lda w1,x        ; save argument 'x' in reg1
  sta reg1,x
  dex
  bpl :-
  lda #<w1
  ldy #>w1
  ldx #<w2
  jsr copy2w
  jsr mul          ; form x^2
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w2
  jsr copy2w
  jsr sub         ; form x^2 - 1
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jsr sqrt        ; w3 = sqrt(x^2 - 1)
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<reg1
  ldy #>reg1
  ldx #<w1
  jsr copy2w
  jsr add         ; w3 = x + sqrt(x^2 - 1)
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jmp loge
.endproc

;                                           1 + x
;   hyperbolic arc tangent:  atanh(x) = ln( -----)
;                                           1 - x
.proc atanh
  ldx #7
  lda w1,x        ; save 'x' in reg1
  sta reg1,x
  dex
  bpl :-
  lda #<unit
  ldy #>unit
  ldx #<w2
  jsr copy2w
  jsr add         ; w3 = 1 + x
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  lda #<reg1
  ldy #>reg1
  ldx #<w2
  jsr copy2w
  ldx #7
: lda w3,x        ; save (1 + x) in reg1
  sta reg1,x
  dex
  bpl :-
  jsr sub         ; w3 = 1 - x
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<reg1
  ldy #>reg1
  ldx #<w1
  jsr copy2w
  jsr div         ; w3 = (1 + x)/(1 - x)
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jsr loge
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<half
  ldy #>half
  ldx #<w2
  jsr copy2w
  jmp mul
.endproc

;
;   hyperbolic arc cosecant: acsch(x) =  asinh(1/x)
;
.proc acsch
  lda #<w1
  ldy #>w1
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jmp asinh
.endproc

;
;   hyperbolic arc secant: asech(x) = acosh(1/x)
;
.proc asech
  lda #<w1
  ldy #>w1
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jmp acosh
.endproc

;
;   hyperbolic arc cotangent: acoth(x) = atanh(1/x)
;
.proc acoth
  lda #<w1
  ldy #>w1
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jmp atanh
.endproc

;
;   log2(x) = loge(x)/loge(2)
;
.proc log2
  jsr loge
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<ln2
  ldy #>ln2
  ldx #<w2
  jsr copy2w
  jmp div
.endproc

;
;   log10(x) = loge(x)/loge(10)
;
.proc log10
  jsr loge
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<ln10
  ldy #>ln10
  ldx #<w2
  jsr copy2w
  jmp div
.endproc

;
;   power(x,y) = x^y = exp(y.loge(x)) = w1^w2
.proc pow
  lda #<w2
  sta ptr1
  lda #>w2
  sta ptr1+1
  lda #<reg3
  sta ptr2
  lda #>reg3
  sta ptr2+1
  jsr copyreg
  jsr loge
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<reg3
  ldy #>reg3
  ldx #<w2
  jsr copy2w
  jsr mul
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  jmp exp
.endproc

;
;   inv -- reciprocal of x = 1/x
;
;   w3 = 1/w1
;
.proc inv
  lda #<w1
  ldy #>w1
  ldx #<w2
  jsr copy2w
  lda #<unit
  ldy #>unit
  ldx #<w1
  jsr copy2w
  jsr div
  rts
.endproc

;
;   abs -- w3 = abs(w1)
;
.proc abs
  lda #<w1
  ldy #>w1
  ldx #<w3
  jsr copy2w
  lda w3
  and #$7f
  sta w3
  rts
.endproc

;
;   chs -- w3 = -w1
;
.proc chs
  lda #<w1
  ldy #>w1
  ldx #<w3
  jsr copy2w
  lda w3
  eor #$80
  sta w3
  rts
.endproc

.proc upang
  ldy #7
  clc
: lda ra+2,y
  bcd_adc (ptr1),y
  sta ra+2,y
  dey
  bpl :-
  rts
.endproc

;
;   shift 'rc' and 'rd' right 'n' digits. Enter with 'n' in 'a-reg'
;
.proc scdnrt
  sta ctr2
  and #1      ; is it an odd number?
  beq scd0    ; no, only do byte shifts
  lda #0      ; yes, do single digit shift
  ldx #7
: ldy rc+2,x
  ora tlh,y
  sta rc+3,x
  lda thl,y
  dex
  bpl :-
  sta rc+2
  lda #0
  ldx #7
: ldy rd+2,x
  ora tlh,y
  sta rd+3,x
  lda thl,y
  dex
  bpl :-
  sta rd+2
scd0:
  lda ctr2
  lsr         ; convert to bytes
  beq scdx    ; exit if no further shift required
  cld
  clc
  bcd_adc #7
  sed
  tay
  ldx #7
: lda rc+2,x
  sta rc+2,y
  lda rd+2,x
  sta rd+2,y
  dey
  dex
  bpl :-
  lda #0
: sta rc+2,y
  sta rd+2,y
  dey
  bpl :-
scdx:
  rts
.endproc

;
;   int -- returns the integer part of a floating point number
;
;   enter with F.P. number in w1, return with result in w3
;
.proc int
  lda #$0f
  bit w1      ; test for sign of exponent and high order exponent digit
  bvs int0    ; negative exponent means integer part is zero
  bne int1    ; large, positive exponent means number is already integer
  jsr int1    ; copy to result register (w3)
  ldx w1+1
  cpx #$12
  bcs intx
  cld
  lda #$b
  sec
  bcd_sbc dec2bin,x   ; number of zeros after decimal point
  sed
  lsr
  beq inta
  tax
  ldy #5
  lda #0
: sta w3+2,y
  dey
  dex
  bne :-
inta:
  bcc intx
  lda #$f0
  and w3+2,y
  sta w3+2,y
intx:
  rts
int0:
  lda #<zero
  ldy #>zero
  ldx #<w3
  jsr copy2w
  rts
int1:
  lda #<w1
  ldy #>w1
  ldx #<w3
  jsr copy2w
  rts
.endproc

;
;   frac -- returns with fractional part of floating point number
;
.proc frac
  ldx #7
  lda w1,x    ; save 'x' in reg3
  sta reg3,x
  dex
  bpl :-
  jsr int     ; find integer part of argument in w1
  lda #<w3     ; move integer part to w2
  ldy #>w3
  ldx #<w2
  jsr copy2w
  lda #<reg3
  ldy #>reg3
  ldx #<w1
  jsr copy2w
  jsr sub     ; subtract integer part from argument for fraction
  lda w3+2
  bne :+
  sta w3
: rts
.endproc

.proc tanscale            ; scale argument to range: 0 #<= arg #<= pi/4
  lda #<twopi
  sta ptr1
  lda #>twopi
  sta ptr1+1
  lda #<scalereg   ; put 2pi in scale register
  sta ptr2
  lda #>scalereg
  sta ptr2+1
  jsr copyreg
  jsr scale       ; get   2pi*frac(arg/2pi), -2pi #<= arg #<= 2pi
  lda w3          ; if arg #< 0 add 2pi
  bpl :+
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<twopi
  ldy #>twopi
  ldx #<w2
  jsr copy2w
  jsr add
: ; now find and save quadrant
  lda #<tpio2
  sta ptr1
  lda #>tpio2      ;   ptr1        ptr2        n z
  sta ptr1+1      ;          =                x 1
  lda #<w3         ;          #>                0 0
  sta ptr2        ;          #<                1 0
  lda #>w3
  sta ptr2+1
  jsr cmpreg      ; 3pi/2 | w3
  bpl :+
  lda #4          ; w3 #> 3pi/2
  sta qdrnt
  jmp tsc1
: lda #<pi
  sta ptr1
  lda #>pi
  sta ptr1+1
  jsr cmpreg     ; pi | w3
  bpl :+
  lda #3         ; w3 #> pi
  sta qdrnt
  jmp tsc1
: lda #<pio2
  sta ptr1
  lda #>pio2
  sta ptr1+1
  jsr cmpreg     ; pi/2 | w3
  bpl :+
  lda #2         ; w3 #> pi/2
  sta qdrnt
  jmp tsc1
: lda #1
  sta qdrnt
tsc1:
  lda qdrnt
  sta ra
  cmp #3
  bcc :+
  lda #<w3         ; map quadrants III and IV to I and II
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<pi
  ldy #>pi
  ldx #<w2
  jsr copy2w
  jsr sub
: lda qdrnt
  and #1
  bne :+
  lda #$80
  sta tnsgn
: lda #<pio2
  sta ptr1
  lda #>pio2
  sta ptr1+1
  jsr cmpreg      ; pi/2 | w3
  bpl :+
  lda #<w3         ; w3 #> pi/2
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<pi
  ldy #>pi
  ldx #<w2
  jsr copy2w
  jsr sub
: lda w3          ; range is: -pi/2 #< arg #< pi/2
  and #$7f        ; take abs value, tnsgn is already correct
  sta w3
  lda #<w3         ; now range is:     0 #<= arg #<= pi/2.
  sta ptr2        ; reduce range to:  0 #<= arg #<= pi/4
  lda #>w3
  sta ptr2+1
  lda #<pio4
  sta ptr1
  lda #>pio4
  sta ptr1+1
  jsr cmpreg      ; pi/4 | w3
  bpl :+          ; if upper part of range, pi/4 #< arg #< pi/2, is
  lda #<pio2       ;  mapped to lower part, 0 #< arg #< pi/4, toggle
  ldy #>pio2       ;  cotangent flag (cotflg).
  ldx #<w1
  jsr copy2w
  lda #<w3
  ldy #>w3
  ldx #<w2
  jsr copy2w
  jsr sub
  lda cotflg
  eor #1
  sta cotflg
: rts             ; all scaling is complete.
.endproc

.proc scale
  lda #<scalereg
  ldy #>scalereg
  ldx #<w2
  jsr copy2w  ; copy scale to w2
  jsr div     ; perform division
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w  ; move result to w1
  jsr frac    ; take fractional portion of result
  lda #<w3
  ldy #>w3
  ldx #<w1
  jsr copy2w
  lda #<scalereg
  ldy #>scalereg
  ldx #<w2
  jsr copy2w  ; copy scale to w2
  jsr mul     ; result is arg mod scale
  rts
.endproc

;
; compare registers pointed to by ptr1 and ptr2.
; returns the following:
;                                       n z
;   zflag = 1 if numbers are the same   x 1
;   nflag = 0 if ptr1 #> ptr2            0 0
;   nflag = 1 if ptr1 #< ptr2            1 0
;
.proc cmpreg
  sed
  ldy #0
  lda (ptr1),y
  eor (ptr2),y
  bpl :+
  ;
  ;   mantissa signs differ, positive number is larger
  ;
  lda (ptr1),y    ; set nflag using ptr1
  ora #1          ; make sure zflag = 0
  rts
  ;
  ;   mantissa signs are the same, check exponent signs
  ;
: asl             ; shift exponent sign into sign bit
  bpl :+
  ;
  ;   exponent signs differ, larger number has:
  ;       mantissa sign positive, exponent sign positive
  ;       mantissa sign negative, exponent sign negative
  ;
  lda (ptr1),y
  bmi negm
  ldy #2
  lda (ptr1),y
  bne nz1
  lda #$80        ; ptr1 = 0 and ptr1 = frac
  rts
nz1:
  lda (ptr2),y
  bne nz2
  rts             ; ptr2 = 0 and ptr1 = frac
nz2:
  ldy #0
  lda (ptr1),y
  asl
  ora #$1
  rts
negm:
  lda (ptr2),y
  asl
  ora #$1
  rts
  ;
  ;   mantissa exponent signs are the same, check exponent magnitude
  ;
: lda (ptr1),y
  eor (ptr2),y
  bne :+
  ;
  ;   first exponent digit matches, check next two
  ;
  iny
  lda (ptr1),y
  eor (ptr2),y
  bne :+
  ;
  ;   all signs and exponents match, compare mantissas
  ;
  ldy #7
  ldx #5
  lda #0
  sta tmp1
  sec
mantst:
  lda (ptr2),y
  bcd_sbc (ptr1),y
  beq zr
  inc tmp1
zr:
  dey
  dex
  bpl mantst
  bcc p1max
  lda tmp1
  bne p2max
  rts
  ;
  ;   all signs match, but exponents differ
  ;
  ;   largest number
  ;       msgn+, esgn+, exponent largest   80.0 #>  1.23
  ;       msgn+, esgn-, exponent smallest  0.33 #>  0.02
  ;       msgn-, esgn+, exponent smallest -1.23 #> -80.0
  ;       msgn-, esgn-, exponent largest  -0.02 #> -0.33
  ;
: ldy #1
  sec
  lda (ptr2),y
  bcd_sbc (ptr1),y
  dey
  lda (ptr2),y
  bcd_sbc (ptr1),y
  bcs e2max
  ;   ptr1 has largest exponent, all signs are the same
  lda (ptr1),y
  and #$c0
  beq p1max
  cmp #$c0
  beq p1max
p2max:
  lda #$81
  rts
p1max:
  lda #1
  rts
  ;   ptr2 has largest exponent, all signs are the same
e2max:
  lda (ptr2),y
  and #$c0
  beq p2max
  cmp #$c0
  beq p2max
  bne p1max
.endproc
; XXX: is it ok to leak to copyreg? is this intended?

.proc copyreg
  ldy #7
: lda (ptr1),y
  sta (ptr2),y
  dey
  bpl :-
  rts
.endproc

.proc clr_r
  ldx #$5f  ; clear all 'rn' regs
  lda #0
: sta ra,x
  dex
  bpl :-
  rts
.endproc

;
;   return with zero
;
;   utility routine to place zero in w3 and return
;
.proc retzr
  lda #<zero
  ldy #>zero
  ldx #<w3
  jsr copy2w
  rts
.endproc

;
;   domain error handler
;
;   returns domain error string with error flags set
;
;       $30,2,0,0,0,0,0,0
;
.proc domnerr
  lda #<dmnerr
  ldy #>dmnerr
  ldx #<w3
  jsr copy2w
  jmp errhndlr
.endproc

;
;   range error handler
;
;   returns range error string with error flags set
;
;       $30,1,0,0,0,0,0,0
;
.proc rangerr
  lda #<rngerr
  ldy #>rngerr
  ldx #<w3
  jsr copy2w
  jmp errhndlr
.endproc

;
;   overflow error handler
;
;   returns overflow error string with error flags set
;
;       $29,$99,$99,$99,$99,$99,$99,$99
;
.proc ovrferr
  lda #<ovferr
  ldy #>ovferr
  ldx #<w3
  jsr copy2w
  jmp errhndlr
.endproc

;
;   divide-by-zero error handler
;
;   returns divide-by-zero error string with error flags set
;
;       $30,3,0,0,0,0,0,0
;
.proc dvzrerr
  lda #<dvzerr
  ldy #>dvzerr
  ldx #<w3
  jsr copy2w
  jmp errhndlr
.endproc

;
;   general error handler
;
;   returns error string in w3 and executes 'rts' instruction
;   NOTE: was brk, maybe rethink later
.proc errhndlr
  rts
.endproc

;
;   The following two tables are used in shifting a BCD register
;   one BCD digit to the right or left.
;
;
;   'tlh' maps the lower decimal digit of its BCD index into
;   the high digit.
;
;   Example:
;
;           ldy #$35
;           lda tlh,y
;
;   Now the 'a' register contains #$50; i.e. the '5' in '35'
;   is shifted into the high digit position and the low
;   digit position is cleared.
;
;   This table, and the one following, support high speed
;   shifting of multibyte BCD registers.
;
.segment "RODATA"

tlh:
.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
.byte 0,$10,$20,$30,$40,$50,$60,$70,$80,$90,0,0,0,0,0,0
;
;   'thl' maps the upper digit of its BCD index into the
;   low digit.
;
;   Example:
;
;           ldy #$35
;           lda thl,y
;
;   Now the 'a' register contains #$03; i.e. the '3' in '35'
;   is shifted to the low digit position and the high digit
;   position is cleared.
;
thl:
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
.byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
.byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
.byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
.byte 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
.byte 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
.byte 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
.byte 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
.byte 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
;
;   'dec2bin' maps a BCD value into its equivalent binary value.
;
;   Example:
;
;           ldy #$35            ; '35' decimal
;           lda dec2bin,y
;
;   The 'a' register now contains 23h = 0010 0011b = 35 decimal
;
dec2bin:
.byte 0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0
.byte $0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,0,0,0,0,0,0
.byte $14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,0,0,0,0,0,0
.byte $1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,0,0,0,0,0,0
.byte $28,$29,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,0,0,0,0,0,0
.byte $32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,0,0,0,0,0,0
.byte $3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,0,0,0,0,0,0
.byte $46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,0,0,0,0,0,0
.byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,0,0,0,0,0,0
.byte $5a,$5b,$5c,$5d,$5e,$5f,$60,$61,$62,$63,0,0,0,0,0,0
;
;   'bin2dec' maps a binary value into its BCD equivalent value.
;
;   Example:
;
;           ldy #$2c            ; '2c' hex = 0010 1100b = 44 decimal
;           lda bin2dec,y
;
;   The 'a' register now contains #$44.
;
bin2dec:
.byte 0,1,2,3,4,5,6,7,8,9
.byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19
.byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29
.byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39
.byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49
.byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59
.byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69
.byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79
.byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89
.byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99
;
;   srblt supports the division of BCD numbers by two
;
;   To use, shift the BCD register right one bit and
;   then replace each byte with the value take from
;   this lookup table using the existing byte as an
;   index.
;
srtbl:   ; shift right table
.byte $00,$01,$02,$03,$04,$05,$06,$07,$05,$06,$07,$08,$09,0,0,0
.byte $10,$11,$12,$13,$14,$15,$16,$17,$15,$16,$17,$18,$19,0,0,0
.byte $20,$21,$22,$23,$24,$25,$26,$27,$25,$26,$27,$28,$29,0,0,0
.byte $30,$31,$32,$33,$34,$35,$36,$37,$35,$36,$37,$38,$39,0,0,0
.byte $40,$41,$42,$43,$44,$45,$46,$47,$45,$46,$47,$49,$49,0,0,0
.byte $50,$51,$52,$53,$54,$55,$56,$57,$55,$56,$57,$58,$59,0,0,0
.byte $60,$61,$62,$63,$64,$65,$66,$67,$65,$66,$67,$68,$69,0,0,0
.byte $70,$71,$72,$73,$74,$75,$76,$77,$75,$76,$77,$78,$79,0,0,0
.byte $50,$51,$52,$53,$54,$55,$56,$57,$55,$56,$57,$58,$59,0,0,0
.byte $60,$61,$62,$63,$64,$65,$66,$67,$65,$66,$67,$68,$69,0,0,0
.byte $70,$71,$72,$73,$74,$75,$76,$77,$75,$76,$77,$78,$79,0,0,0
.byte $80,$81,$82,$83,$84,$85,$86,$87,$85,$86,$87,$88,$89,0,0,0
.byte $90,$91,$92,$93,$94,$95,$96,$97,$95,$96,$97,$98,$99,0,0,0
sltbl:   ; shift left table
.byte $00,$01,$02,$03,$04,$08,$09,$0a,$0b,$0c,0,0,0,0,0,0
.byte $10,$11,$12,$13,$14,$18,$19,$1a,$1b,$1c,0,0,0,0,0,0
.byte $20,$21,$22,$23,$24,$28,$29,$2a,$2b,$2c,0,0,0,0,0,0
.byte $30,$31,$32,$33,$34,$38,$39,$3a,$3b,$3c,0,0,0,0,0,0
.byte $40,$41,$42,$43,$44,$48,$49,$4a,$4b,$4c,0,0,0,0,0,0
.byte $80,$81,$82,$83,$84,$88,$89,$8a,$8b,$8c,0,0,0,0,0,0
.byte $90,$91,$92,$93,$94,$98,$99,$9a,$9b,$9c,0,0,0,0,0,0
.byte $a0,$a1,$a2,$a3,$a4,$a8,$a9,$aa,$ab,$ac,0,0,0,0,0,0
.byte $b0,$b1,$b2,$b3,$b4,$b8,$b9,$ba,$bb,$bc,0,0,0,0,0,0
.byte $c0,$c1,$c2,$c3,$c4,$c8,$c9,$ca,$cb,$cc,0,0,0,0,0,0
hex2asc:
.byte '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
.byte "0123456789ABCDEF"

.segment "BSS"
mtbl: .res 16
mtbl1: .res 16
mtbl2: .res 16*8

.segment "RODATA"
.export zero
.export ln2
.export ln10
.export iln10
.export exp0
.export sqt2
.export pi
.export twopi
.export sqp5
.export invpi
.export i2pi
.export pio2
.export npio2
.export tpio2
.export pio4
.export unit
.export half
.export nrpi2
.export hnth
.export ovferr
.export rngerr
.export dmnerr
.export dvzerr
;
;   miscellaneous constants in floating
;   point format
;
zero:   .byte $00,$00,$00,$00,$00,$00,$00,$00
ln2:    .byte $40,$01,$69,$31,$47,$18,$05,$60
ln10:   .byte $00,$00,$23,$02,$58,$50,$92,$99
iln10:  .byte $40,$01,$43,$42,$94,$48,$19,$03
exp0:   .byte $00,$00,$27,$18,$28,$18,$28,$46
sqt2:   .byte $00,$00,$14,$14,$21,$35,$62,$37
pi:     .byte $00,$00,$31,$41,$59,$26,$53,$59
twopi:  .byte $00,$00,$62,$83,$18,$53,$07,$18
sqp5:   .byte $40,$01,$70,$71,$06,$78,$11,$87
invpi:  .byte $40,$01,$31,$83,$09,$88,$61,$84
i2pi:   .byte $40,$01,$15,$91,$54,$94,$30,$92
pio2:   .byte $00,$00,$15,$70,$79,$63,$26,$79
npio2:  .byte $80,$00,$15,$70,$79,$63,$26,$79
tpio2:  .byte $00,$00,$47,$12,$38,$89,$80,$38
pio4:   .byte $40,$01,$78,$53,$98,$16,$33,$98
unit:   .byte $00,$00,$10,$00,$00,$00,$00,$00
half:   .byte $40,$01,$50,$00,$00,$00,$00,$00
nrpi2:  .byte $00,$15,$70,$79,$00,$00,$00,$00
hnth:   .byte $40,$02,$10,$00,$00,$00,$00,$00
ovferr: .byte $29,$99,$99,$99,$99,$99,$99,$99
rngerr: .byte $30,1,0,0,0,0,0,0
dmnerr: .byte $30,2,0,0,0,0,0,0
dvzerr: .byte $30,3,0,0,0,0,0,0
;
;   Coefficient table for cosine by polynomial evaluation
;
kcos:
.byte  $40,$05,$23,$15,$39,$31,$67,$00
.byte  $c0,$03,$13,$85,$37,$04,$26,$40
.byte  $40,$02,$41,$66,$35,$84,$67,$69
.byte  $c0,$01,$49,$99,$99,$05,$34,$55
.byte  $40,$01,$99,$99,$99,$95,$34,$64

;
;   atn(1), atn(0.1), atn(0.01), atn(0.001), ...
;
;   This table is in floating point format
;
katn:
.byte $40,$01,$78,$53,$98,$16,$33,$97
.byte $40,$02,$99,$66,$86,$52,$49,$12
.byte $40,$03,$99,$99,$66,$66,$86,$67
.byte $40,$04,$99,$99,$99,$66,$66,$67
.byte $40,$05,$99,$99,$99,$99,$66,$67
.byte $40,$06,$99,$99,$99,$99,$99,$67
.byte $40,$06,$10,0,0,0,0,0
.byte $40,$07,$10,0,0,0,0,0

;
;   atn(1), atn(0.1), atn(0.01), atn(0.001), ...
;
;   This table is in fixed point format
;
kxatn:
.byte $07,$85,$39,$81,$63,$39,$74,$48
.byte $00,$99,$66,$86,$52,$49,$11,$62
.byte $00,$09,$99,$96,$66,$68,$66,$65
.byte $00,$00,$99,$99,$99,$66,$66,$67
.byte $00,$00,$09,$99,$99,$99,$96,$67
.byte $00,$00,$01,$00,$00,$00,$00,$00
.byte $00,$00,$00,$10,$00,$00,$00,$00
.byte $00,$00,$00,$01,$00,$00,$00,$00
.byte $00,$00,$00,$00,$10,$00,$00,$00
.byte $00,$00,$00,$00,$01,$00,$00,$00
.byte $00,$00,$00,$00,$00,$10,$00,$00
.byte $00,$00,$00,$00,$00,$01,$00,$00

;
;   ln(2), ln(1.1), ln(1.01),ln(1.001),...
;
;   This table is in floating point format
;
klog:
.byte $40,$01,$69,$31,$47,$18,$05,$60
.byte $40,$02,$95,$31,$01,$79,$80,$43
.byte $40,$03,$99,$50,$33,$08,$53,$17
.byte $40,$04,$99,$95,$00,$33,$30,$84
.byte $40,$05,$99,$99,$50,$00,$33,$33
.byte $40,$06,$99,$99,$95,$00,$00,$33
.byte $40,$07,$99,$99,$99,$50,$00,$00
.byte $40,$08,$99,$99,$99,$95,$00,$00
.byte $40,$09,$99,$99,$99,$99,$50,$00
.byte $40,$10,$99,$99,$99,$99,$95,$00
.byte $40,$11,$99,$99,$99,$99,$99,$50

;
;   ln(2), ln(1.1), ln(1.01), ln(1.001),...
;
;   This table is in fixed decimal format
;
kxlog:
.byte $06,$93,$14,$71,$80,$55,$99,$45
.byte $00,$95,$31,$01,$79,$80,$43,$25
.byte $00,$09,$95,$03,$30,$85,$31,$68
.byte $00,$00,$99,$95,$00,$33,$30,$84
.byte $00,$00,$09,$99,$95,$00,$03,$33
.byte $00,$00,$00,$99,$99,$95,$00,$00
.byte $00,$00,$00,$09,$99,$99,$95,$00
.byte $00,$00,$00,$00,$99,$99,$99,$95
.byte $00,$00,$00,$00,$10,$00,$00,$00
.byte $00,$00,$00,$00,$01,$00,$00,$00
.byte $00,$00,$00,$00,$00,$10,$00,$00
.byte $00,$00,$00,$00,$00,$01,$00,$00
.byte $00,$00,$00,$00,$00,$00,$10,$00
.byte $00,$00,$00,$00,$00,$00,$01,$00
.byte $00,$00,$00,$00,$00,$00,$00,$10
.byte $00,$00,$00,$00,$00,$00,$00,$01
kln:
.byte $C0,$01,$25,$00,$00,$00,$00,$00
.byte $40,$01,$33,$33,$33,$33,$33,$33
.byte $C0,$01,$50,$00,$00,$00,$00,$00
.byte $00,$00,$10,$00,$00,$00,$00,$00
tst1: .byte $00,$01,$80,$00,$00,$00,$00,$00
tst2: .byte $00,$01,$80,$00,$00,$00,$00,$01
tst3: .byte $80,$01,$80,$00,$00,$00,$00,$00
tst4: .byte $80,$00,$12,$30,$00,$00,$00,$00
tst5: .byte $40,$01,$33,$00,$00,$00,$00,$00
tst6: .byte $40,$02,$20,$00,$00,$00,$00,$00
tst7: .byte $C0,$01,$33,$00,$00,$00,$00,$00
tst8: .byte $C0,$02,$20,$00,$00,$00,$00,$00
