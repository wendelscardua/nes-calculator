; information from https://wiki.nesdev.com/w/index.php/INES

.segment "HEADER"
.byte "NES", $1a ; Magic string that always begins an iNES header
.byte $02        ; Number of 16KB PRG-ROM banks
.byte $01        ; Number of 8KB CHR-ROM banks

.byte %00000001  ; Flags 6
;      ||||||||
;      |||||||+- Mirroring: 0: horizontal (vertical arrangement) (CIRAM A10 = PPU A11)
;      |||||||              1: vertical (horizontal arrangement) (CIRAM A10 = PPU A10)
;      ||||||+-- 1: Cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
;      |||||+--- 1: 512-byte trainer at $7000-$71FF (stored before PRG data)
;      ||||+---- 1: Ignore mirroring control or above mirroring bit; instead provide four-screen VRAM
;      ++++----- Lower nybble of mapper number

.byte %00000000  ; Flags 7
;      ||||||||
;      |||||||+- VS Unisystem
;      ||||||+-- PlayChoice-10 (8KB of Hint Screen data stored after CHR data)
;      ||||++--- If equal to 2, flags 8-15 are in NES 2.0 format
;      ++++----- Upper nybble of mapper number

.byte $00        ; PRG-RAM size

.byte $00        ; NTSC format
