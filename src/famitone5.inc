.ifndef _FAMITONE5_INC_
  _FAMITONE5_INC_ = 1

  .include "audio-data.inc"
  .include "macros.inc"

  .segment "RODATA"

  .import ft_sfx_ch0
  .import ft_sfx_ch1
  .import ft_sfx_ch2
  .import ft_sfx_ch3

  .segment "ZEROPAGE"

  .import current_song

  .macro SFX effect, channel
    save_regs
    LDA #sfx::effect
    LDX .ident ( .concat( "ft_sfx_", .string(channel) ) )
    JSR FamiToneSfxPlay
    restore_regs
  .endmacro

  .macro PLAY track
    save_regs
    LDA #music_track::track
    JSR FamiToneMusicPlay
    restore_regs
  .endmacro

  .macro PLAY_IF_NOT track
    .local skip
    save_regs
    LDA #music_track::track
    CMP current_song
    BEQ skip
    STA current_song
    JSR FamiToneMusicPlay
skip:
    restore_regs
  .endmacro

  .macro STOP_IF_NOT track
    .local skip
    save_regs
    LDA #music_track::track
    CMP current_song
    BEQ skip
    JSR FamiToneMusicStop
skip:
    restore_regs
  .endmacro

  .segment "CODE"

  .import FamiToneInit, FamiToneSfxInit
  .import FamiToneMusicPlay, FamiToneSfxPlay
  .import FamiToneMusicStop, FamiToneMusicPause
  .import FamiToneUpdate

.endif
