# haskell-halogen-sound

Play looping themes, shuffled albums and sound effects in the browser. The player preloads upcoming tracks and can play effects over the current music.

Define your application's sounds as an enumeration:

```haskell
data Sfx = MenuTheme | Campaign1 | Campaign2 | Campaign3 | Dice | Battle
  deriving stock (Eq, Ord, Enum, Bounded)

instance Sound Sfx where
  soundUrl s = "assets/audio/" <> show s <> ".mp3"
  -- Fetched as the player starts and never let go: they must play at once.
  persistent s = s `elem` [MenuTheme, Dice, Battle]
```

```haskell
player <- newPlayer browser defaultConfig :: IO (Player Sfx)

playTheme player MenuTheme                          -- the menu: stop, a pause, loop this
playAlbum player [Campaign1, Campaign2, Campaign3]  -- a game: shuffled, on and on
playEffect player Dice                              -- over whatever plays
stopMusic player
setMuted player True                                -- and nothing is fetched while muted
setMusicVolume player 0.2                           -- a slider each: reaches the music playing
setEffectsVolume player 0.8
```

For sound collections discovered at runtime, such as an album fetched from a server, implement `persistentSounds` directly instead of relying on an enumeration.

Only one music track plays at a time. `playTheme`, `playAlbum` and `stopMusic` replace it immediately and can be called from any thread. Albums play every track once per shuffled round, avoid consecutive repeats and insert `gap` seconds of silence before each track.

While a track plays, the player fetches the next `bufferAhead` tracks. Non-persistent files beyond `cacheSize` are evicted in least-recently-used order, except those currently playing or buffered. The player does not retain the entire album.

Music and effects have separate volumes between 0 and 1, initially set in `Config`. Changes apply immediately. Zero music volume stops playback and further music requests until raised again, while persistent sounds continue preloading and stay cached. Effects at zero volume are skipped.

The browser backend fetches complete files at low priority, creates blob URLs and plays them through HTML audio. If playback requires user interaction, voices wait for the first click or key. Native playback is inert. The player logic uses a `Backend` record, so tests can supply a recording implementation; see `test/Test/Player.hs`.

To restore music after a reload, save `musicPosition player` and `musicPaused player`. Create a new player, call `pauseMusic` first if playback was paused, then call `playAlbumFrom player tracks track seconds`. The saved track resumes at that position. The remaining tracks are shuffled, with each track still played once per round. If the saved track is no longer present, playback starts as a new album. Custom backends must apply `Voicing.offset` before the sound becomes audible.

`node sound/test/browser-offset.cjs` checks metadata, seeking and paused voices in both browser implementations. Run the player tests with `cabal test haskell-halogen-sound` on each backend.
