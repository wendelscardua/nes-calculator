.segment "RODATA"
.export metatile_ul, metatile_ur, metatile_dl, metatile_dr, metatile_pal
metatile_ul: .byte $60, $62, $64, $66, $68, $00, $82, $80, $82, $84, $86, $88
metatile_ur: .byte $61, $63, $65, $67, $69, $00, $83, $81, $85, $83, $87, $89
metatile_dl: .byte $70, $72, $74, $76, $78, $00, $90, $92, $92, $94, $96, $98
metatile_dr: .byte $71, $73, $75, $77, $79, $00, $91, $93, $95, $93, $97, $99
metatile_pal: .byte $00, $01, $02, $01, $03, $00, $02, $02, $02, $02, $02, $02
