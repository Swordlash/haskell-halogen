# haskell-halogen-sound

Music and sound effects for a page: a theme to loop, an album played in a
shuffled order with the next track fetched ahead, and effects on top.

The sounds are an enumeration of the application's:

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
```

- A type whose sounds are not all known in advance (an album read from a
  server) is not an enumeration: it says `persistentSounds` itself.
- There is one piece of music at a time; `playTheme`, `playAlbum` and
  `stopMusic` replace it at once, from any thread.
- An album plays every track once a round, each round shuffled, never the
  same track twice in a row, with `gap` seconds of silence before each.
- While a track plays, the next `bufferAhead` are fetched. Files that are not
  persistent are let go, least recently used first, beyond `cacheSize`
  (besides those playing or buffered), so an album is never held whole.
- `browser` fetches whole files at low priority into blob URLs and plays them
  with HTML audio. A page may not play before its first click or key; a voice
  started earlier waits for one. Natively it plays nothing.
- The logic runs over a `Backend` record, so a test can hand in one that
  only writes down what it was asked (see `test/Test/Player.hs`).
