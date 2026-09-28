# Revision history for haskell-halogen-sound

## Unreleased

* `skipTrack`: on to the album's next track at once, without the gap (the
  album's order goes on from there; nothing for a theme or silence).
* `nowPlaying`: the track playing now, if any (not during a gap), for a
  page to show its name.

## 0.2.0.0

* `setMusicVolume` and `setEffectsVolume`: a volume each for the music and
  the effects, from 0 to 1, changed while the player runs (the ones in
  `Config` are where they start). A change reaches the music playing at
  once. At zero music volume, playback stops and the music makes no further
  track requests, and it comes back as it was asked for once the volume is
  raised; persistent sounds continue to preload and remain cached. Effects
  at 0 are not played.
* **Breaking:** `Backend` has `setVolume`, to change a playing voice's
  volume. The browser backend sets the audio element's.

## 0.1.0.0

* First version: `Sound` class, a player (theme, shuffled album with
  buffering ahead, effects, a least-recently-used cache, mute), and a
  browser backend.
