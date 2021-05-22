.ifndef _FAMITONE5_CONFIG_INC_
  _FAMITONE5_CONFIG_INC_ = 1

  ; famitone5 config
  FT_DPCM_OFF		= $fc00	;$c000..$ffc0, 64-byte steps
  FT_SFX_STREAMS	= 4		;number of sound effects played at once, 1..4

  FT_DPCM_ENABLE	= 0	;undefine to exclude all DMC code
  FT_SFX_ENABLE	= 1		;undefine to exclude all sound effects code
  FT_THREAD		= 1		;undefine if you are calling sound effects from the same thread as the sound update call

  FT_PAL_SUPPORT	= 1			;undefine to exclude PAL support
  FT_NTSC_SUPPORT	= 1			;undefine to exclude NTSC support
.endif
