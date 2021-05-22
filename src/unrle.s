; RLE decompressor based on Shiru's
.include "constants.inc"

.segment "ZEROPAGE"

.export rle_ptr

rle_ptr: .res 2
rle_tag: .res 1
rle_byte: .res 1

.segment "CODE"

.export unrle
.proc unrle
  ; Decompress data from address on rle_ptr to PPUDATA
  LDY #0
  JSR fetch_rle_byte
  STA rle_tag
fetch_command:
  JSR fetch_rle_byte
  CMP rle_tag
  BEQ tag_found
  STA PPUDATA
  STA rle_byte
  BNE fetch_command
tag_found:
  JSR fetch_rle_byte
  CMP #0
  BEQ the_end
  TAX
  LDA rle_byte
write_rle:
  STA PPUDATA
  DEX
  BNE write_rle
  JMP fetch_command
the_end:
  RTS
.endproc

.proc fetch_rle_byte
  LDA (rle_ptr), Y
  INC rle_ptr
  BNE :+
  INC rle_ptr+1
:
  RTS
.endproc
