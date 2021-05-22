.ifndef _VRAM_BUFFER_INC_
  _VRAM_BUFFER_INC_ = 1

  .segment "ZEROPAGE"

  .import vram_buffer_sp
  .import main_sp

  .segment "STACK"

  .import vram_buffer

  .segment "CODE"

  .import flush_vram_buffer

  .segment "RODATA"

  .import _tens
  .import _ones

  .macro switch_to_vram_stack
    TSX
    STX main_sp
    LDX #0
    TXS
  .endmacro

  .macro switch_to_main_stack
    LDX main_sp
    TXS
  .endmacro

  .macro reset_vram_stack
    PHA
    LDA #0
    STA vram_buffer_sp
    STA vram_buffer
    PLA
  .endmacro

  ; ensures we have "size" bytes free on vram buffer
  ; cobbles X
  ; returns: X = first free buffer index
  .macro vb_alloc size
    .local wait_nmi
    .local no_flush
    .local retry
retry:
    LDX vram_buffer_sp
    CPX #($7f - size)
    BCC no_flush
    LDA nmis
wait_nmi:
    CMP nmis
    BEQ wait_nmi
    JMP retry
no_flush:
    ; "cheating": X has the buffer position,
    ; vb_close with write the new position back
    ; meanwhile we zero the sp, so nmi won't
    ; try flushing an incomplete buffer
    LDA #$00
    STA vram_buffer_sp
  .endmacro

  ; cobbles X
  .macro write_tile_to_vram ppu_addr, tile
    vb_alloc 5
    vb_ppu_addr ppu_addr
    vb_push tile
    vb_push #$00
    vb_close
  .endmacro

  ; input: ppuaddr and 2-digit decimal
  ; cobbles X, Y, A
  .macro write_decimal_to_vram ppu_addr, decimal
    vb_alloc 6
    vb_ppu_addr ppu_addr
    LDA decimal
    TAY
    vb_push {_tens, Y}
    vb_push {_ones, Y}
    vb_push #$00
    vb_close
  .endmacro

  .macro write_string_to_vram ppu_addr, str_ptr
    PHA
    TXA
    PHA

    vb_alloc 5

    vb_ppu_addr (ppu_addr | $8000)
    vb_push_word str_ptr
    vb_close

    PLA
    TAX
    PLA
  .endmacro

  .macro write_palette_to_vram ppu_addr, c1, c2, c3
    PHA
    TXA
    PHA

    vb_alloc 7

    vb_push #.hibyte(ppu_addr)
    vb_push #.lobyte(ppu_addr)
    vb_push c1
    vb_push c2
    vb_push c3
    vb_push #$ff
    vb_close

    PLA
    TAX
    PLA
  .endmacro

  ; use after vb_alloc
  ; pushes a byte to the buffer
  .macro vb_push byte
    LDA byte
    vb_pusha
  .endmacro

  ; use after vb_alloc
  ; pushes A to the buffer
  .macro vb_pusha
    STA vram_buffer, X
    INX
  .endmacro

  ; use after pushing enough bytes to buffer
  ; sets guard byte and current buffer size
  .macro vb_close
    LDA #$00
    STA vram_buffer, X
    STX vram_buffer_sp
  .endmacro

  ; pushes ppu address to vram buffer
  .macro vb_ppu_addr ppu_addr, horizontal
    vb_push #.hibyte(ppu_addr)
    vb_push #.lobyte(ppu_addr)
  .endmacro

  .macro vb_push_word word
    vb_push #.lobyte(word)
    vb_push #.hibyte(word)
  .endmacro
.endif
