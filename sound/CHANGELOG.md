# Revision history for haskell-halogen-sound

## 0.2.0.0

* `setMusicVolume` and `setEffectsVolume`: a volume each for the music and
  the effects, from 0 to 1, changed while the player runs (the ones in
  `Config` are where they start). A change reaches the music playing at
  once. At 0 the music stops and is not fetched, and comes back as it was
  asked for once the volume is raised; effects at 0 are not played.
* **Breaking:** `Backend` has `setVolume`, to change a playing voice's
  volume. The browser backend sets the audio element's.

## 0.1.0.0

* First version: `Sound` class, a player (theme, shuffled album with
  buffering ahead, effects, a least-recently-used cache, mute), and a
  browser backend.
