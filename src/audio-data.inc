.ifndef _AUDIO_DATA_INC_
  _AUDIO_DATA_INC_ = 1

  ; music/sfx constants
  .enum music_track
    LogoARhythm
  .endenum

  .enum sfx
    SwitchPlus
    SwitchMinus
    Match
    Matcher
    Matchest
    TrophyGet
    Pause
    Unpause
  .endenum

  .segment "RODATA"

  .import music_data
  .import sfx_data

.endif
