{-# LANGUAGE MultiWayIf #-}

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
--
-- The music and the effects have a volume each ('setMusicVolume',
-- 'setEffectsVolume'), which a page can offer as two sliders: a change
-- reaches the music playing at once. At zero music volume, playback stops
-- and the music makes no further track requests, and it comes back as it
-- was asked for when the volume is raised; persistent sounds go on
-- preloading and stay cached, whatever the volume (persistence is about
-- the cache, the volume about playing). Effects at 0 are not played.
module Halogen.Sound
  ( Sound (..)
  , Config (..)
  , defaultConfig
  , Player
  , newPlayer
  , playEffect
  , playTheme
  , playAlbum
  , skipTrack
  , previousTrack
  , pauseMusic
  , resumeMusic
  , musicPaused
  , nowPlaying
  , musicProgress
  , stopMusic
  , setMuted
  , setMusicVolume
  , setEffectsVolume
  , module Halogen.Sound.Backend
  )
where

import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Halogen.Sound.Backend
import Halogen.Sound.Order
import Protolude

-- | An application's sounds.
--
-- Usually an enumeration, and then only 'soundUrl' and 'persistent' need
-- saying. A type with values not known in advance (an album read from a
-- server: @Track 1@, @Track 2@, …) says 'persistentSounds' instead.
class (Ord t) => Sound t where
  -- | Where the file is.
  soundUrl :: t -> Text

  -- | Fetched as soon as the player starts, and never let go: what has to
  -- play at once when asked (a menu theme, the effects).
  persistent :: t -> Bool
  persistent _ = False

  -- | Every persistent sound.
  persistentSounds :: [t]
  default persistentSounds :: (Enum t, Bounded t) => [t]
  persistentSounds = filter persistent [minBound .. maxBound]

  -- | A factor on the music or effects volume, to even out loud files.
  loudness :: t -> Double
  loudness _ = 1

data Config = Config
  { musicVolume :: Double
  -- ^ From 0 to 1, until 'setMusicVolume'.
  , effectsVolume :: Double
  -- ^ From 0 to 1, until 'setEffectsVolume'.
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
  , levels :: IORef Levels
  , nowRef :: IORef (Maybe (Int, t))
  -- ^ The track the music of this number is playing, for 'nowPlaying'.
  , restRef :: IORef (Maybe (Int, Order t))
  -- ^ What the album of this number plays after its current track, for
  -- 'skipTrack'.
  , historyRef :: IORef (Int, [t])
  -- ^ The tracks the album of this number has started, the latest first
  -- (the one playing or waiting out its gap), for 'previousTrack'.
  , pausedRef :: IORef Bool
  -- ^ The music is held ('pauseMusic'): a voice started now starts held.
  -- Set under the control lock, read under the music's voice lock.
  , musicVoice :: MVar (Maybe (Int, voice, Double))
  -- ^ The music's voice, the number of the music playing it, and the
  -- sound's 'loudness'. Also the lock under which the music's volume is
  -- read to start a voice and changed, so that no change falls between.
  }

data Levels = Levels {musicLevel :: Double, effectsLevel :: Double}

-- | The files, and what may not be let go.
data Store t clip = Store
  { entries :: Map t (Entry clip)
  , pins :: Map t Int
  -- ^ Effects playing, counted.
  , wanted :: (Int, Set t)
  -- ^ What the music of this number plays and buffers.
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
  , playing :: Int
  -- ^ Numbers each piece of music started, so that one stopping does not
  -- clear what its successor wants. (Not the thread's id: comparing those
  -- is missing from GHC's JavaScript runtime.)
  }

-- | A player, which starts fetching the persistent sounds.
newPlayer :: forall t clip voice. (Sound t) => Backend clip voice -> Config -> IO (Player t)
newPlayer backend config = do
  store <- newMVar Store {entries = mempty, pins = mempty, wanted = (0, mempty), clock = 0}
  control <- newMVar Control {music = Silence, thread = Nothing, muted = config.startMuted, shuffles = config.seed, playing = 0}
  levels <- newIORef Levels {musicLevel = clamp config.musicVolume, effectsLevel = clamp config.effectsVolume}
  musicVoice <- newMVar Nothing
  nowRef <- newIORef Nothing
  restRef <- newIORef Nothing
  historyRef <- newIORef (0, [])
  pausedRef <- newIORef False
  let engine = Engine {backend, config, store, control, levels, nowRef, restRef, historyRef, pausedRef, musicVoice}
  unless config.startMuted (preloadPersistent engine)
  pure (Player engine)

-- | Play a sound once, on top of anything playing.
playEffect :: (Sound t) => Player t -> t -> IO ()
playEffect (Player e) t = do
  c <- readMVar e.control
  level <- (.effectsLevel) <$> readIORef e.levels
  unless (c.muted || level <= 0) $ void $ forkIO $ quietly $ bracket_ (pin e t 1) (pin e t (-1) >> evict e) $ do
    clip <- obtain e t
    for_ clip $ \x -> playVoice e x Voicing {volume = level * loudness t, looping = False}

-- | Stop the music, and after a 'gap' loop this track.
playTheme :: (Sound t) => Player t -> t -> IO ()
playTheme (Player e) t = setMusic e (Theme t)

-- | Stop the music, and play these tracks in a shuffled order, with a 'gap'
-- before each, until told otherwise.
playAlbum :: (Sound t) => Player t -> [t] -> IO ()
playAlbum (Player e) ts = setMusic e (Album ts)

-- | Skip the album's track playing, or the one waiting out its 'gap', and
-- play the one after it at once. Each call skips one more. Nothing for a
-- theme, silence, or music not playing (muted, at no volume).
skipTrack :: (Sound t) => Player t -> IO ()
skipTrack (Player e) = modifyMVar_ e.control $ \c -> do
  rest <- readIORef e.restRef
  case (c.music, c.thread, rest) of
    (Album _, Just th, Just (m, order)) | m == c.playing -> do
      -- What the new thread plays and buffers is wanted before the old one
      -- lets its files go, so a track already fetched plays at once.
      want e (c.playing + 1) (take (1 + e.config.bufferAhead) (upcoming (1 + e.config.bufferAhead) order))
      killThread th
      -- The new thread plays the first of @order@; a skip right after this
      -- one, before it starts, finds what comes after that.
      atomicWriteIORef e.restRef (Just (c.playing + 1, afterFirst order))
      -- The tracks started so far stay behind the new one, for a step back.
      atomicModifyIORef' e.historyRef (\(hm, ts) -> ((c.playing + 1, if hm == c.playing then ts else []), ()))
      start e c {thread = Nothing} (\n -> album e n False order)
    _ -> pure c

-- | Back to the album's track before the one playing, at once; or, when
-- this one has played for three seconds or more (or is the album's first),
-- this one again from its start. Each call goes one further back. Nothing
-- for a theme, silence, or music not playing.
previousTrack :: (Sound t) => Player t -> IO ()
previousTrack (Player e) = modifyMVar_ e.control $ \c -> do
  rest <- readIORef e.restRef
  (hn, history) <- readIORef e.historyRef
  at <- modifyMVar e.musicVoice $ \v -> case v of
    Just (m, voice, _) | m == c.playing -> (v,) . maybe 0 fst <$> e.backend.voiceProgress voice
    _ -> pure (v, 0)
  case (c.music, c.thread, rest) of
    (Album _, Just th, Just (m, order)) | m == c.playing, hn == c.playing, current : earlier <- history -> do
      let (again, before) = case earlier of
            prev : older | at < 3 -> ([prev, current], older)
            _ -> ([current], earlier)
          order' = playFirst again order
      want e (c.playing + 1) (take (1 + e.config.bufferAhead) (upcoming (1 + e.config.bufferAhead) order'))
      killThread th
      atomicWriteIORef e.restRef (Just (c.playing + 1, afterFirst order'))
      atomicWriteIORef e.historyRef (c.playing + 1, before)
      start e c {thread = Nothing} (\n -> album e n False order')
    _ -> pure c

-- | Hold the music where it is: the track playing stops where it is, and
-- one that starts meanwhile (after a gap, a new album) starts held.
pauseMusic :: Player t -> IO ()
pauseMusic (Player e) = modifyMVar_ e.control $ \c -> do
  atomicWriteIORef e.pausedRef True
  withMVar e.musicVoice $ \v -> for_ v $ \(_, voice, _) -> e.backend.pauseVoice voice
  pure c

-- | Go on with the music held by 'pauseMusic', from where it was.
resumeMusic :: Player t -> IO ()
resumeMusic (Player e) = modifyMVar_ e.control $ \c -> do
  atomicWriteIORef e.pausedRef False
  withMVar e.musicVoice $ \v -> for_ v $ \(_, voice, _) -> e.backend.resumeVoice voice
  pure c

-- | Whether the music is held.
musicPaused :: Player t -> IO Bool
musicPaused (Player e) = readIORef e.pausedRef

-- | The track playing now, if any (not during the 'gap' before one).
nowPlaying :: Player t -> IO (Maybe t)
nowPlaying (Player e) = do
  c <- readMVar e.control
  now <- readIORef e.nowRef
  pure $ case now of
    Just (m, t) | m == c.playing, isJust c.thread -> Just t
    _ -> Nothing

-- | How far the music's track has played and how long it is, in seconds.
musicProgress :: Player t -> IO (Maybe (Double, Double))
musicProgress (Player e) = do
  c <- readMVar e.control
  voice <- readMVar e.musicVoice
  case voice of
    Just (m, v, _) | m == c.playing, isJust c.thread -> e.backend.voiceProgress v
    _ -> pure Nothing

stopMusic :: (Sound t) => Player t -> IO ()
stopMusic (Player e) = setMusic e Silence

-- | Silence everything, or bring back the music that was asked for last.
-- While muted, nothing is fetched.
setMuted :: (Sound t) => Player t -> Bool -> IO ()
setMuted (Player e) m = modifyMVar_ e.control $ \c -> case (m, c.muted) of
  (True, False) -> for_ c.thread killThread >> pure c {muted = True, thread = Nothing}
  (False, True) -> preloadPersistent e >> run e c {muted = False}
  _ -> pure c

-- | The music's volume, from 0 to 1, for the music playing too. At 0 the
-- music stops and asks for no further tracks, and starts again when the
-- volume is raised. Persistent sounds are preloaded and kept regardless.
setMusicVolume :: (Sound t) => Player t -> Double -> IO ()
setMusicVolume (Player e) v = modifyMVar_ e.control $ \c -> do
  let new = clamp v
  old <- modifyMVar e.musicVoice $ \playing -> do
    before <- atomicModifyIORef' e.levels (\l -> (l {musicLevel = new}, l.musicLevel))
    for_ playing $ \(_, voice, louder) -> e.backend.setVolume voice (new * louder)
    pure (playing, before)
  if
    | new <= 0 && old > 0 -> for_ c.thread killThread >> pure c {thread = Nothing}
    | new > 0 && old <= 0 && isNothing c.thread -> run e c
    | otherwise -> pure c

-- | The effects' volume, from 0 to 1, for the effects played from now on.
setEffectsVolume :: Player t -> Double -> IO ()
setEffectsVolume (Player e) v = atomicModifyIORef' e.levels (\l -> (l {effectsLevel = clamp v}, ()))

clamp :: Double -> Double
clamp = max 0 . min 1

----------------------------------------------------------------------
-- The music

setMusic :: (Sound t) => Engine t clip voice -> Music t -> IO ()
setMusic e m = modifyMVar_ e.control $ \c -> do
  for_ c.thread killThread
  -- The album replaced lets go of its tracks.
  atomicWriteIORef e.restRef Nothing
  run e c {music = m, thread = Nothing}

-- | Start the thread for the music asked for, unless muted or at no volume.
run :: (Sound t) => Engine t clip voice -> Control t -> IO (Control t)
run e c = do
  level <- (.musicLevel) <$> readIORef e.levels
  if c.muted || level <= 0 then pure c else case c.music of
      Silence -> pure c
      Theme t -> start e c (\n -> theme e n t)
      Album ts -> do
        let order = newOrder c.shuffles ts
        -- Known before this returns, for a skip right after it.
        atomicWriteIORef e.restRef (Just (c.playing + 1, afterFirst order))
        start e c {shuffles = c.shuffles + 1} (\n -> album e n True order)

-- | Start a thread for a piece of music, numbered anew.
start :: (Sound t) => Engine t clip voice -> Control t -> (Int -> IO ()) -> IO (Control t)
start e c act = do
  let n = c.playing + 1
  th <- forkIO (quietly (act n) `finally` unwant e n)
  pure c {thread = Just th, playing = n}

theme :: (Sound t) => Engine t clip voice -> Int -> t -> IO ()
theme e n t = do
  want e n [t]
  -- Fetched during the silence, if it is not at hand already.
  prefetch e t
  pause e.config.gap
  clip <- obtain e t
  for_ clip $ \x -> playMusicVoice e n x (loudness t) True (nowOn e n t) (notPlaying e n) `finally` notPlaying e n

-- | Play an album's tracks in its order, a 'gap' before each (but the
-- first, when skipped to).
album :: (Sound t) => Engine t clip voice -> Int -> Bool -> Order t -> IO ()
album e n gapFirst order = for_ (nextTrack order) $ \(t, order') -> do
  let ahead = upcoming e.config.bufferAhead order'
  -- A skip, during the gap or the track, goes on to the one after it.
  atomicWriteIORef e.restRef (Just (n, order'))
  -- And a step back returns to this one, or the one before it.
  atomicModifyIORef' e.historyRef (\(m, ts) -> ((n, t : if m == n then ts else []), ()))
  want e n (t : ahead)
  prefetch e t
  when gapFirst (pause e.config.gap)
  clip <- obtain e t
  -- The next ones only once this one is here, so as not to hold it up.
  for_ ahead (prefetch e)
  case clip of
    -- Played out: the next track waits out its gap, and a skip then skips it.
    Just x -> playMusicVoice e n x (loudness t) False (nowOn e n t) (notPlaying e n >> atomicWriteIORef e.restRef (Just (n, afterFirst order'))) `finally` notPlaying e n
    -- Not to be had: on to the next, but not in a spin should none be.
    Nothing -> pause 1
  album e n True order'

-- | An order past its next track.
afterFirst :: (Eq t) => Order t -> Order t
afterFirst order = maybe order snd (nextTrack order)

-- | The music of this number plays this track now.
nowOn :: Engine t clip voice -> Int -> t -> IO ()
nowOn e n t = atomicWriteIORef e.nowRef (Just (n, t))

-- | The music of this number plays nothing now (a successor's track stays).
notPlaying :: Engine t clip voice -> Int -> IO ()
notPlaying e n = atomicModifyIORef' e.nowRef (\case Just (m, _) | m == n -> (Nothing, ()); other -> (other, ()))

-- | Play a voice to its end (forever, for a loop), and stop it however
-- this ends: a stopped thread takes its voice with it.
playVoice :: Engine t clip voice -> clip -> Voicing -> IO ()
playVoice e clip voicing = do
  done <- newEmptyMVar
  bracket
    (e.backend.startVoice clip voicing pass (void (tryPutMVar done ())))
    e.backend.stopVoice
    (\_ -> takeMVar done)

-- | 'playVoice' for the music of this number, at the music's volume, with
-- the voice at hand for 'setMusicVolume' while it plays.
--
-- @started@ runs when the voice is heard (the backend says when), @ended@
-- as soon as it has played out, before its teardown.
playMusicVoice :: Engine t clip voice -> Int -> clip -> Double -> Bool -> IO () -> IO () -> IO ()
playMusicVoice e n clip louder looping started ended = do
  done <- newEmptyMVar
  bracket
    ( modifyMVar e.musicVoice $ \_ -> do
        level <- (.musicLevel) <$> readIORef e.levels
        voice <- e.backend.startVoice clip Voicing {volume = level * louder, looping} started (void (tryPutMVar done ()))
        held <- readIORef e.pausedRef
        when held (e.backend.pauseVoice voice)
        pure (Just (n, voice, louder), voice)
    )
    -- Retired under the same lock a volume change takes: the voice leaves
    -- the registration and is stopped with no change in between, and the
    -- registration goes even if the stop fails. A successor's registration
    -- (another number) stays.
    ( \voice -> do
        stopped <- modifyMVar e.musicVoice $ \current -> do
          r <- try (e.backend.stopVoice voice)
          let rest = case current of
                Just (m, _, _) | m == n -> Nothing
                other -> other
          pure (rest, r)
        either (throwIO :: SomeException -> IO ()) pure stopped
    )
    (\_ -> takeMVar done >> ended)

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
  void $ forkIO $ quietly $ for_ persistentSounds (obtain e)

pin :: (Ord t) => Engine t clip voice -> t -> Int -> IO ()
pin e t n = modifyMVar_ e.store $ \s ->
  pure s {pins = Map.filter (> 0) (Map.insertWith (+) t n s.pins)}

-- | What the music of this number plays and buffers; the rest may go.
want :: (Sound t) => Engine t clip voice -> Int -> [t] -> IO ()
want e n ts = do
  modifyMVar_ e.store $ \s -> pure s {wanted = (n, Set.fromList ts)}
  evict e

-- | The music of this number has stopped. Music that has replaced it
-- meanwhile keeps its own.
unwant :: (Sound t) => Engine t clip voice -> Int -> IO ()
unwant e n = do
  modifyMVar_ e.store $ \s -> pure $ if fst s.wanted == n then s {wanted = (0, mempty)} else s
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
