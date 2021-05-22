.linecont +
.include "graphics.inc"
.include "macros.inc"
.include "unrle.inc"
.include "game-states/playing.inc"

.segment "ZEROPAGE"

addr_ptr: .res 2

.segment "CODE"

.export load_level_data
.proc load_level_data
  LDX current_puzzle

  LDA level_data_pointers_l, X
  STA addr_ptr
  LDA level_data_pointers_h, X
  STA addr_ptr+1

  LDY #0 ; Y iterates over level data

  ; read and load nametables

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA (addr_ptr), Y
  INY
  STA rle_ptr
  LDA (addr_ptr), Y
  INY
  STA rle_ptr+1
  save_regs
  JSR metatile_unrle

  LDX #$00
left_attr_loop:
  JSR fetch_rle_byte
  STA PPUDATA
  STA attributes, X
  INX
  CPX #$40
  BNE left_attr_loop

  restore_regs

  ; read turtle
  LDA (addr_ptr), Y
  INY
  STA turtle_x
  LDA (addr_ptr), Y
  INY
  STA turtle_y
  LDA (addr_ptr), Y
  INY
  STA turtle_direction

  .repeat 3, i
    LDA (addr_ptr), Y
    INY
    STA strawberry_x + i
    LDA (addr_ptr), Y
    INY
    STA strawberry_y + i
  .endrepeat

  ; command ppu addr
  LDA (addr_ptr), Y
  INY
  STA command_ppu_addr
  LDA (addr_ptr), Y
  INY
  STA command_ppu_addr + 1

  ; command queue
  LDX #$00
command_loop:
  LDA (addr_ptr), Y
  INY
  STA command_queue, X
  INX
  CMP #$ff
  BNE command_loop

  LDA #$00
  STA command_queue_head
  STA command_queue_tail

  ; read bg matrix pointer
  LDA (addr_ptr), Y
  INY
  STA bg_matrix_ptr

  LDA (addr_ptr), Y
  INY
  STA bg_matrix_ptr+1

  ; read poison matrix pointer
  LDA (addr_ptr), Y
  INY
  STA poison_matrix_ptr

  LDA (addr_ptr), Y
  INY
  STA poison_matrix_ptr+1

  ; read boing matrix pointer
  LDA (addr_ptr), Y
  INY
  STA boing_matrix_ptr

  LDA (addr_ptr), Y
  INY
  STA boing_matrix_ptr+1

  ; read goal matrix pointer
  LDA (addr_ptr), Y
  INY
  STA goal_matrix_ptr

  LDA (addr_ptr), Y
  INY
  STA goal_matrix_ptr+1

  RTS
.endproc

.segment "RODATA"

.define level_data_pointers level_01_data, \
                            level_02_data, \
                            level_03_data, \
                            level_04_data, \
                            level_05_data, \
                            level_win_data
level_data_pointers_l: .lobytes level_data_pointers
level_data_pointers_h: .hibytes level_data_pointers

; level data format:
; "nametable" pointer (metatile.rle + attrs)
; turtle x, y, direction
; strawberry 1 x, y
; strawberry 2 x, y
; strawberry 3 x, y
; (x, y in metatile coordinates)
; command queue (ending with -1)
; bg matrix pointer
; poison matrix pointer
; boing matrix pointer
; goal matrix pointer
;

.include "../assets/maps/level-01.inc"
.include "../assets/maps/level-02.inc"
.include "../assets/maps/level-03.inc"
.include "../assets/maps/level-04.inc"
.include "../assets/maps/level-05.inc"
.include "../assets/maps/level-win.inc"
