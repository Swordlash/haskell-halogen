# Revision history for haskell-halogen-sound

## Unreleased

* `musicProgress`: how far the music's track has played and how long it
  is, for a page to show.
* **Breaking:** `Backend` has `voiceProgress`.
* The browser backend ends a voice whose file cannot be played (an error
  event, or a refusal other than the page's want of a first click), so an
  album goes on to its next track instead of waiting forever.

## Unreleased

* `skipTrack`: skips the album's track playing, or the one waiting out its
  gap, and plays the one after it at once; each call skips one more, also
  in quick succession. Nothing for a theme or silence.
* `nowPlaying`: the track playing now, if any: set once its voice is
  heard, cleared as soon as it plays out (not during a gap).
* **Breaking:** `Backend`'s `startVoice` takes an action to call when the
  voice is heard, besides the one for its end. The browser backend calls
  it on the audio element's `playing` event, so a voice waiting for the
  page's first click does not count as playing.

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
