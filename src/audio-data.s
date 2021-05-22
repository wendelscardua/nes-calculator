.segment "RODATA"

music_data:
.include "../audio/soundtrack.s"

sfx_data:
.include "../audio/sfx.s"

.export music_data
.export sfx_data
