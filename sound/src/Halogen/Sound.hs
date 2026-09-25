-- | Music and sound effects for a page: a theme to loop, an album to play
-- in a shuffled order, and effects on top, over any 'Backend'.
--
-- The sounds are a type of the application's, an enumeration each of whose
-- values names a file ('Sound'). A player keeps a cache of fetched files:
-- the persistent ones (a menu theme, the effects) are fetched as soon as
-- the player starts and kept, the others are fetched when wanted and let go
-- again, least recently used first, once more than 'cacheSize' of them are
-- held. An album fetches the track after the one playing ('bufferAhead')
-- while it plays, so no track waits on the network and no album is held in
-- memory whole.
--
-- There is one piece of music at a time. 'playTheme', 'playAlbum' and
-- 'stopMusic' each replace whatever was playing, at once, from any thread:
-- the music runs on a thread of its own, which they stop. Effects play on
-- top of it, each on its own voice.
module Halogen.Sound
  ( Sound (..)
  , Config (..)
  , defaultConfig
  , Player
  , newPlayer
  , playEffect
  , playTheme
  , playAlbum
  , stopMusic
  , setMuted
  , module Halogen.Sound.Backend
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Halogen.Sound.Backend
import Halogen.Sound.Order
import Protolude

-- | An application's sounds.
class (Ord t, Enum t, Bounded t) => Sound t where
  -- | Where the file is.
  soundUrl :: t -> Text

  -- | Fetched as soon as the player starts, and never let go: what has to
  -- play at once when asked (a menu theme, the effects).
  persistent :: t -> Bool
  persistent _ = False

  -- | A factor on the music or effects volume, to even out loud files.
  loudness :: t -> Double
  loudness _ = 1

data Config = Config
  { musicVolume :: Double
  , effectsVolume :: Double
  , gap :: Double
  -- ^ Seconds of silence before a piece of music, and between the tracks of
  -- an album.
  , bufferAhead :: Int
  -- ^ How many of an album's next tracks are fetched while one plays.
  , cacheSize :: Int
  -- ^ How many files that are not persistent are held at most, besides
  -- those playing or buffered.
  , seed :: Word64
  -- ^ For the shuffles.
  , startMuted :: Bool
  }

defaultConfig :: Config
defaultConfig =
  Config
    { musicVolume = 0.35
    , effectsVolume = 0.6
    , gap = 2
    , bufferAhead = 1
    , cacheSize = 2
    , seed = 0x5eed
    , startMuted = False
    }

data Music t = Silence | Theme t | Album [t]

data Player t = forall clip voice. Player (Engine t clip voice)

data Engine t clip voice = Engine
  { backend :: Backend clip voice
  , config :: Config
  , store :: MVar (Store t clip)
  , control :: MVar (Control t)
  }

-- | The files, and what may not be let go.
data Store t clip = Store
  { entries :: Map t (Entry clip)
  , pins :: Map t Int
  -- ^ Effects playing, counted.
  , wanted :: (Maybe ThreadId, Set t)
  -- ^ What the music thread named plays and buffers.
  , clock :: Int
  -- ^ Counts uses, for least recently used.
  }

data Entry clip = Loading (MVar (Maybe clip)) | Ready clip Int

data Control t = Control
  { music :: Music t
  , thread :: Maybe ThreadId
  , muted :: Bool
  , shuffles :: Word64
  -- ^ The seed of the next album's shuffle.
  }

-- | A player, which starts fetching the persistent sounds.
newPlayer :: forall t clip voice. (Sound t) => Backend clip voice -> Config -> IO (Player t)
newPlayer backend config = do
  store <- newMVar Store {entries = mempty, pins = mempty, wanted = (Nothing, mempty), clock = 0}
  control <- newMVar Control {music = Silence, thread = Nothing, muted = config.startMuted, shuffles = config.seed}
  let engine = Engine {backend, config, store, control}
  unless config.startMuted (preloadPersistent engine)
  pure (Player engine)

-- | Play a sound once, on top of anything playing.
playEffect :: (Sound t) => Player t -> t -> IO ()
playEffect (Player e) t = do
  c <- readMVar e.control
  unless c.muted $ void $ forkIO $ quietly $ bracket_ (pin e t 1) (pin e t (-1) >> evict e) $ do
    clip <- obtain e t
    for_ clip $ \x -> playVoice e x Voicing {volume = e.config.effectsVolume * loudness t, looping = False}

-- | Stop the music, and after a 'gap' loop this track.
playTheme :: (Sound t) => Player t -> t -> IO ()
playTheme (Player e) t = setMusic e (Theme t)

-- | Stop the music, and play these tracks in a shuffled order, with a 'gap'
-- before each, until told otherwise.
playAlbum :: (Sound t) => Player t -> [t] -> IO ()
playAlbum (Player e) ts = setMusic e (Album ts)

stopMusic :: (Sound t) => Player t -> IO ()
stopMusic (Player e) = setMusic e Silence

-- | Silence everything, or bring back the music that was asked for last.
-- While muted, nothing is fetched.
setMuted :: (Sound t) => Player t -> Bool -> IO ()
setMuted (Player e) m = modifyMVar_ e.control $ \c -> case (m, c.muted) of
  (True, False) -> for_ c.thread killThread >> pure c {muted = True, thread = Nothing}
  (False, True) -> preloadPersistent e >> run e c {muted = False}
  _ -> pure c

----------------------------------------------------------------------
-- The music

setMusic :: (Sound t) => Engine t clip voice -> Music t -> IO ()
setMusic e m = modifyMVar_ e.control $ \c -> do
  for_ c.thread killThread
  run e c {music = m, thread = Nothing}

-- | Start the thread for the music asked for, unless muted.
run :: (Sound t) => Engine t clip voice -> Control t -> IO (Control t)
run e c
  | c.muted = pure c
  | otherwise = case c.music of
      Silence -> pure c
      Theme t -> start c (theme e t)
      Album ts -> start c {shuffles = c.shuffles + 1} (album e (newOrder c.shuffles ts))
  where
    start c' act = do
      th <- forkIO (quietly act `finally` unwant e)
      pure c' {thread = Just th}

theme :: (Sound t) => Engine t clip voice -> t -> IO ()
theme e t = do
  want e [t]
  -- Fetched during the silence, if it is not at hand already.
  prefetch e t
  pause e.config.gap
  clip <- obtain e t
  for_ clip $ \x -> playVoice e x Voicing {volume = e.config.musicVolume * loudness t, looping = True}

album :: (Sound t) => Engine t clip voice -> Order t -> IO ()
album e order = for_ (nextTrack order) $ \(t, order') -> do
  let ahead = upcoming e.config.bufferAhead order'
  want e (t : ahead)
  prefetch e t
  pause e.config.gap
  clip <- obtain e t
  -- The next ones only once this one is here, so as not to hold it up.
  for_ ahead (prefetch e)
  case clip of
    Just x -> playVoice e x Voicing {volume = e.config.musicVolume * loudness t, looping = False}
    -- Not to be had: on to the next, but not in a spin should none be.
    Nothing -> pause 1
  album e order'

-- | Play a voice to its end (forever, for a loop), and stop it however
-- this ends: a stopped thread takes its voice with it.
playVoice :: Engine t clip voice -> clip -> Voicing -> IO ()
playVoice e clip voicing = do
  done <- newEmptyMVar
  bracket
    (e.backend.startVoice clip voicing (void (tryPutMVar done ())))
    e.backend.stopVoice
    (\_ -> takeMVar done)

----------------------------------------------------------------------
-- The files

-- | A file, fetched now if it is not at hand; waits for it.
obtain :: (Sound t) => Engine t clip voice -> t -> IO (Maybe clip)
obtain e t = join (request e t)

-- | Start fetching a file if it is not at hand, without waiting for it.
prefetch :: (Sound t) => Engine t clip voice -> t -> IO ()
prefetch e t = void (request e t)

-- | Mark a use of a file, and return how to wait for it. A fetch runs on a
-- thread of its own, so that a caller stopped while waiting (the music,
-- replaced) does not leave it half done for the others waiting on it.
request :: (Sound t) => Engine t clip voice -> t -> IO (IO (Maybe clip))
request e t = modifyMVar e.store $ \s -> do
  let s' = s {clock = s.clock + 1}
  case Map.lookup t s.entries of
    Just (Ready clip _) -> pure (s' {entries = Map.insert t (Ready clip s.clock) s.entries}, pure (Just clip))
    Just (Loading v) -> pure (s', readMVar v)
    Nothing -> do
      v <- newEmptyMVar
      void $ forkIO (load v)
      pure (s' {entries = Map.insert t (Loading v) s.entries}, readMVar v)
  where
    load v = do
      clip <- e.backend.fetchClip (soundUrl t) `catch` \(_ :: SomeException) -> pure Nothing
      modifyMVar_ e.store $ \s ->
        pure
          s
            { entries = maybe (Map.delete t) (\x -> Map.insert t (Ready x s.clock)) clip s.entries
            , clock = s.clock + 1
            }
      putMVar v clip
      evict e

preloadPersistent :: (Sound t) => Engine t clip voice -> IO ()
preloadPersistent e =
  -- One at a time, so that they share the connection with the rest of the
  -- page rather than take it over.
  void $ forkIO $ quietly $ for_ (filter persistent [minBound .. maxBound]) (obtain e)

pin :: (Ord t) => Engine t clip voice -> t -> Int -> IO ()
pin e t n = modifyMVar_ e.store $ \s ->
  pure s {pins = Map.filter (> 0) (Map.insertWith (+) t n s.pins)}

-- | What the calling thread's music plays and buffers; the rest may go.
want :: (Sound t) => Engine t clip voice -> [t] -> IO ()
want e ts = do
  me <- myThreadId
  modifyMVar_ e.store $ \s -> pure s {wanted = (Just me, Set.fromList ts)}
  evict e

-- | The music on the calling thread has stopped. A thread that has replaced
-- it meanwhile keeps its own.
unwant :: (Sound t) => Engine t clip voice -> IO ()
unwant e = do
  me <- myThreadId
  modifyMVar_ e.store $ \s -> pure $ if fst s.wanted == Just me then s {wanted = (Nothing, mempty)} else s
  evict e

-- | Let the least recently used files go, beyond 'cacheSize'.
evict :: (Sound t) => Engine t clip voice -> IO ()
evict e = do
  victims <- modifyMVar e.store $ \s -> do
    let held t = persistent t || Map.member t s.pins || Set.member t (snd s.wanted)
        counted = Map.size (Map.filterWithKey (\t _ -> not (held t)) s.entries)
        free = sortOn (\(_, _, used) -> used) [(t, clip, used) | (t, Ready clip used) <- Map.toList s.entries, not (held t)]
        victims = take (counted - e.config.cacheSize) free
    pure (s {entries = foldr (\(t, _, _) -> Map.delete t) s.entries victims}, victims)
  for_ victims $ \(_, clip, _) -> e.backend.releaseClip clip

----------------------------------------------------------------------

pause :: Double -> IO ()
pause seconds = when (seconds > 0) $ threadDelay (round (seconds * 1000000))

-- | A player's threads end quietly, whether stopped or failed: a sound that
-- cannot be played is not worth an error.
quietly :: IO () -> IO ()
quietly act = act `catch` \(_ :: SomeException) -> pass
