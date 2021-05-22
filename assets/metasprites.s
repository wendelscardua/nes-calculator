.include "../src/graphics.inc"

.linecont +
.feature force_range

.segment "RODATA"

.export metasprites_l, metasprites_h

up_turtle_metasprite:

.byte - 8,- 8,$00,0
.byte   0,- 8,$01,0
.byte - 8,  0,$10,0
.byte   0,  0,$11,0
.byte 128

down_turtle_metasprite:

.byte - 8,- 8,$10,0|OAM_FLIP_V
.byte   0,- 8,$11,0|OAM_FLIP_V
.byte - 8,  0,$00,0|OAM_FLIP_V
.byte   0,  0,$01,0|OAM_FLIP_V
.byte 128

left_turtle_metasprite:

.byte - 8,- 8,$02,0
.byte - 8,  0,$12,0
.byte   0,- 8,$03,0
.byte   0,  0,$13,0
.byte 128

right_turtle_metasprite:

.byte - 8,- 8,$03,0|OAM_FLIP_H
.byte - 8,  0,$13,0|OAM_FLIP_H
.byte   0,- 8,$02,0|OAM_FLIP_H
.byte   0,  0,$12,0|OAM_FLIP_H
.byte 128

strawberry_metasprite:
.byte - 4,- 4,$04,1
.byte 128

.define metasprite_pointers up_turtle_metasprite, \
                            down_turtle_metasprite, \
                            left_turtle_metasprite, \
                            right_turtle_metasprite, \
                            strawberry_metasprite

metasprites_l: .lobytes metasprite_pointers
metasprites_h: .hibytes metasprite_pointers
