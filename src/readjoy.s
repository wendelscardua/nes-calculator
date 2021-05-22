.include "constants.inc"

; we reserve one byte for storing the data that is read from controller
.zeropage
buttons: .res 2
last_frame_buttons: .res 2
released_buttons: .res 2
pressed_buttons: .res 2

; buttons layout:
; A B Select Start Up Down Left Right

.export buttons
.export last_frame_buttons
.export pressed_buttons
.export released_buttons

.segment "CODE"

; At the same time that we strobe bit 0, we initialize the ring counter
; so we're hitting two birds with one stone here
.export readjoy
.proc readjoy
    lda buttons
    sta last_frame_buttons
    lda buttons+1
    sta last_frame_buttons+1
    lda #$01
    ; While the strobe bit is set, buttons will be continuously reloaded.
    ; This means that reading from JOYPAD1 will only return the state of the
    ; first button: button A.
    sta JOYPAD1
    sta buttons
    lsr a        ; now A is 0
    ; By storing 0 into JOYPAD1, the strobe bit is cleared and the reloading stops.
    ; This allows all 8 buttons (newly reloaded) to be read from JOYPAD1.
    sta JOYPAD1
loop:
    lda JOYPAD2
    lsr a
    rol buttons+1
    lda JOYPAD1
    lsr a	       ; bit 0 -> Carry
    rol buttons  ; Carry -> bit 0; bit 7 -> Carry
    bcc loop
    lda buttons
    eor #%11111111
    and last_frame_buttons
    sta released_buttons
    lda last_frame_buttons
    eor #%11111111
    and buttons
    sta pressed_buttons

    lda buttons+1
    eor #%11111111
    and last_frame_buttons+1
    sta released_buttons+1
    lda last_frame_buttons+1
    eor #%11111111
    and buttons+1
    sta pressed_buttons+1
    rts
.endproc
