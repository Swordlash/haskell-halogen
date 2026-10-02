-- | The player, over a backend that plays nothing and writes down what it
-- is asked: the test ends the tracks itself.
module Test.Player (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.IORef
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Halogen.Sound
import Prelude
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

data Snd = Menu | Click | T1 | T2 | T3 | T4
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Sound Snd where
  soundUrl = T.pack . show
  persistent = (`elem` [Menu, Click])

-- | An album known only when the program runs (read from a server, say).
data Open = Title | Track Int
  deriving stock (Eq, Ord, Show)

instance Sound Open where
  soundUrl = \case
    Title -> "Title"
    Track n -> T.pack ("track-" <> show n)
  persistentSounds = [Title]
  persistent = (== Title)

album :: [Snd]
album = [T1, T2, T3, T4]

data Event = Fetched Text | Released Text | Started Text Bool | Stopped Text | Volume Text Double | DeadVolume Text | Positioned Text Double | Paused Text | Resumed Text
  deriving stock (Eq, Show)

data Fake = Fake
  { events :: IORef [Event]
  , voices :: IORef (Map.Map Int (Text, IO ()))
  , counter :: IORef Int
  , dead :: IORef (Map.Map Int Text)
  -- ^ Voices stopped: a volume change on one is a bug ('DeadVolume').
  , held :: IORef (Map.Map Int (IO ()))
  -- ^ Voices started held: what says they are heard, on resuming.
  , positions :: IORef (Map.Map Int Double)
  , stopGate :: IORef (Maybe (MVar (), MVar ()))
  -- ^ When set, a stop marks its voice dead, signals the first, and waits
  -- for the second before it finishes.
  }

newFake :: IO (Fake, Backend Text Int)
newFake = do
  fake <- Fake <$> newIORef [] <*> newIORef mempty <*> newIORef 0 <*> newIORef mempty <*> newIORef mempty <*> newIORef mempty <*> newIORef Nothing
  let note e = atomicModifyIORef' fake.events (\es -> (es <> [e], ()))
      backend =
        Backend
          { fetchClip = \url -> threadDelay 1000 >> note (Fetched url) >> pure (Just url)
          , releaseClip = note . Released
          , startVoice = \clip Voicing {volume, looping, held = startHeld, offset} heard ended -> do
              n <- atomicModifyIORef' fake.counter (\i -> (i + 1, i))
              atomicModifyIORef' fake.voices (\vs -> (Map.insert n (clip, ended) vs, ()))
              note (Started clip looping)
              note (Positioned clip offset)
              atomicModifyIORef' fake.positions (\ps -> (Map.insert n offset ps, ()))
              note (Volume clip volume)
              -- Heard at once, as a backend may be, unless started held.
              if startHeld
                then note (Paused clip) >> atomicModifyIORef' fake.held (\hs -> (Map.insert n heard hs, ()))
                else heard
              pure n
          , stopVoice = \n -> do
              v <- atomicModifyIORef' fake.voices (\vs -> (Map.delete n vs, Map.lookup n vs))
              mapM_ (\(clip, _) -> atomicModifyIORef' fake.dead (\ds -> (Map.insert n clip ds, ()))) v
              readIORef fake.stopGate >>= mapM_ (\(reached, release) -> putMVar reached () >> takeMVar release)
              mapM_ (note . Stopped . fst) v
          , pauseVoice = \n -> Map.lookup n <$> readIORef fake.voices >>= mapM_ (note . Paused . fst)
          , resumeVoice = \n -> do
              Map.lookup n <$> readIORef fake.voices >>= mapM_ (note . Resumed . fst)
              atomicModifyIORef' fake.held (\hs -> (Map.delete n hs, Map.lookup n hs)) >>= sequence_
          , voiceProgress = \n -> do
              v <- Map.lookup n <$> readIORef fake.voices
              at <- Map.findWithDefault 0 n <$> readIORef fake.positions
              pure (fmap (const (max 1 at, 60)) v)
          , setVolume = \n volume -> do
              gone <- Map.lookup n <$> readIORef fake.dead
              case gone of
                Just clip -> note (DeadVolume clip)
                Nothing -> do
                  v <- Map.lookup n <$> readIORef fake.voices
                  mapM_ (\(clip, _) -> note (Volume clip volume)) v
          }
  pure (fake, backend)

config :: Config
config = defaultConfig {gap = 0.005, bufferAhead = 1, cacheSize = 1}

-- | Wait (up to two seconds) for the log to satisfy a condition.
eventually :: Fake -> ([Event] -> Bool) -> IO [Event]
eventually fake ok = go (400 :: Int)
  where
    go n = do
      es <- readIORef fake.events
      if ok es || n == 0 then pure es else threadDelay 5000 >> go (n - 1)

lastOf :: [a] -> Maybe a
lastOf = foldl (\_ x -> Just x) Nothing

startsOf :: [Event] -> [Text]
startsOf es = [t | Started t _ <- es]

-- | End the track playing (the one voice that does not loop).
finishTrack :: Fake -> IO ()
finishTrack fake = do
  vs <- readIORef fake.voices
  sequence_ [ended | (clip, ended) <- Map.elems vs, clip `notElem` ["Menu", "Click"]]

-- | Let an album play @n@ tracks, ending each; the tracks started.
playTracks :: Fake -> Int -> IO [Text]
playTracks fake n = go 0
  where
    go k
      | k == n = startsOf <$> readIORef fake.events
      | otherwise = do
          _ <- eventually fake (\es -> length (startsOf es) > k)
          finishTrack fake
          go (k + 1)

held :: [Event] -> [Text]
held = foldl step []
  where
    step hs (Fetched t) | t `notElem` ["Menu", "Click"] = t : hs
    step hs (Released t) = filter (/= t) hs
    step hs _ = hs

spec :: Spec
spec = describe "player" $ do
  it "resumes the chosen track at its position, then starts later tracks at zero" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config {gap = 5}
    playAlbumFrom player album T3 23.5
    es <- eventually fake (elem (Positioned "T3" 23.5))
    startsOf es `shouldBe` ["T3"]
    musicPosition player >>= (`shouldBe` Just (T3, 23.5))
    skipTrack player
    es' <- eventually fake (\xs -> length (startsOf xs) >= 2)
    let second = mconcat (take 1 (drop 1 (startsOf es')))
    second `shouldSatisfy` (/= "T3")
    Positioned second 0 `elem` es' `shouldBe` True
    previousTrack player
    es'' <- eventually fake (\xs -> length (startsOf xs) >= 3)
    take 1 (drop 2 (startsOf es'')) `shouldBe` ["T3"]
    Positioned "T3" 0 `elem` es'' `shouldBe` True
    stopMusic player
    musicPosition player >>= (`shouldBe` Nothing)

  it "captures a restored track while it starts held, before it has been heard" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    pauseMusic player
    playAlbumFrom player album T2 17
    es <- eventually fake (elem (Paused "T2"))
    Positioned "T2" 17 `elem` es `shouldBe` True
    nowPlaying player >>= (`shouldBe` Nothing)
    musicPosition player >>= (`shouldBe` Just (T2, 17))
    resumeMusic player
    es' <- eventually fake (elem (Resumed "T2"))
    Resumed "T2" `elem` es' `shouldBe` True
    nowPlaying player >>= (`shouldBe` Just T2)
    stopMusic player

  it "defers a saved position while muted and at zero volume" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config {startMuted = True, musicVolume = 0}
    playAlbumFrom player album T4 31
    setMuted player False
    threadDelay 20000
    startsOf <$> readIORef fake.events >>= (`shouldBe` [])
    setMusicVolume player 0.5
    es <- eventually fake (elem (Positioned "T4" 31))
    startsOf es `shouldBe` ["T4"]
    stopMusic player

  mapM_
    ( \(label, silence, restart) ->
        it ("consumes the saved position before restarting music after " <> label) $ do
          (fake, backend) <- newFake
          player <- newPlayer backend config {startMuted = True, musicVolume = 0}
          playAlbumFrom player album T4 31
          setMuted player False
          setMusicVolume player 0.5
          es <- eventually fake (elem (Positioned "T4" 31))
          [at | Positioned _ at <- es] `shouldBe` [31]
          skipTrack player
          es' <- eventually fake (\xs -> length [() | Positioned _ _ <- xs] >= 2)
          [at | Positioned _ at <- es'] `shouldBe` [31, 0]
          silence player
          restart player
          es'' <- eventually fake (\xs -> length [() | Positioned _ _ <- xs] >= 3)
          [at | Positioned _ at <- es''] `shouldBe` [31, 0, 0]
          stopMusic player
    )
    [ ("muting", \player -> setMuted player True, \player -> setMuted player False)
    , ("zero volume", \player -> setMusicVolume player 0, \player -> setMusicVolume player 0.5)
    ]

  it "starts an ordinary album if the saved track is no longer in it" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playAlbumFrom player album Menu 31
    es <- eventually fake (not . null . startsOf)
    startsOf es `shouldSatisfy` all (`elem` ["T1", "T2", "T3", "T4"])
    [at | Positioned _ at <- es] `shouldBe` [0]
    stopMusic player

  it "normalizes invalid positions to the beginning" $ do
    mapM_
      ( \at -> do
          (fake, backend) <- newFake
          player <- newPlayer backend config
          playAlbumFrom player album T1 at
          es <- eventually fake (elem (Positioned "T1" 0))
          Positioned "T1" 0 `elem` es `shouldBe` True
          stopMusic player
      )
      [-10, 0 / 0, 1 / 0]

  it "fetches the persistent sounds as it starts" $ do
    (fake, backend) <- newFake
    _ <- newPlayer backend config :: IO (Player Snd)
    es <- eventually fake (\es -> Fetched "Menu" `elem` es && Fetched "Click" `elem` es)
    es `shouldSatisfy` (\xs -> Fetched "Menu" `elem` xs && Fetched "Click" `elem` xs)

  it "plays each track of an album once a round, never twice in a row" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playAlbum player album
    starts <- playTracks fake 12
    let rounds = [take 4 (drop (4 * r) starts) | r <- [0 .. 2]]
    map sort rounds `shouldBe` replicate 3 ["T1", "T2", "T3", "T4"]
    and (zipWith (/=) starts (drop 1 starts)) `shouldBe` True
    stopMusic player

  it "fetches the next track while one plays, and lets old ones go" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playAlbum player album
    let check k = do
          _ <- eventually fake (\es -> length (startsOf es) > k)
          -- The next one is fetched before this one ends: two tracks held.
          es <- eventually fake (\xs -> length (nub (held xs)) >= 2)
          length (nub (held es)) `shouldSatisfy` (\h -> h >= 2 && h <= 3)
          finishTrack fake
    mapM_ check [0 .. 7]
    es <- readIORef fake.events
    -- Playing, buffered, and one more at most.
    length (held es) `shouldSatisfy` (<= 3)
    [t | Released t <- es] `shouldSatisfy` (not . null)
    [t | Released t <- es, t `elem` ["Menu", "Click"]] `shouldBe` []
    stopMusic player

  it "says what plays, and skips to the next track at once" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config {gap = 5}
    nowPlaying player >>= (`shouldBe` Nothing)
    playAlbum player album
    -- Not until the gap before the first track has passed.
    threadDelay 50000
    nowPlaying player >>= (`shouldBe` Nothing)
    skipTrack player
    es <- eventually fake (not . null . startsOf)
    let first = mconcat (take 1 (startsOf es))
    fmap (T.pack . show) <$> nowPlaying player >>= (`shouldBe` Just first)
    -- The next one, without the five-second gap, and not the same one.
    skipTrack player
    es' <- eventually fake (\xs -> length (startsOf xs) >= 2)
    Stopped first `elem` es' `shouldBe` True
    let second = mconcat (take 1 (drop 1 (startsOf es')))
    second `shouldSatisfy` (/= first)
    fmap (T.pack . show) <$> nowPlaying player >>= (`shouldBe` Just second)
    -- Played out: nothing plays during the gap before the next one.
    finishTrack fake
    threadDelay 20000
    nowPlaying player >>= (`shouldBe` Nothing)
    stopMusic player
    nowPlaying player >>= (`shouldBe` Nothing)

  it "skips one track a call, also twice in a row right after the album starts" $ do
    -- The same seed plays the same order: one player plays it through,
    -- the other skips twice before its first track.
    (plain, backend) <- newFake
    through <- newPlayer backend config
    playAlbum through album
    order <- playTracks plain 3
    stopMusic through
    (fake, backend') <- newFake
    player <- newPlayer backend' config {gap = 5}
    playAlbum player album
    skipTrack player
    skipTrack player
    es <- eventually fake (not . null . startsOf)
    startsOf es `shouldBe` drop 2 order
    stopMusic player

  it "plays a track already fetched at once when skipped to, not fetching it again" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config {gap = 5, cacheSize = 0}
    playAlbum player album
    -- Straight to the first track; the one after it is fetched meanwhile.
    skipTrack player
    es <- eventually fake (\xs -> not (null (startsOf xs)) && length (held xs) >= 2)
    let first = mconcat (take 1 (startsOf es))
        next = mconcat (take 1 [t | t <- held es, t /= first])
    skipTrack player
    es' <- eventually fake (\xs -> length (startsOf xs) >= 2)
    drop 1 (startsOf es') `shouldBe` [next]
    length [t | Fetched t <- es', t == next] `shouldBe` 1
    stopMusic player

  it "goes back to the track before the one playing, and on to this one after it" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playAlbum player album
    -- Two tracks: the first played out, the second playing (for a second).
    starts <- playTracks fake 1
    es <- eventually fake (\xs -> length (startsOf xs) >= 2)
    let first = mconcat (take 1 (startsOf es))
        second = mconcat (take 1 (drop 1 (startsOf es)))
    previousTrack player
    es' <- eventually fake (\xs -> length (startsOf xs) >= 3)
    Stopped second `elem` es' `shouldBe` True
    take 1 (drop 2 (startsOf es')) `shouldBe` [first]
    starts `shouldBe` [first]
    -- Played out, the one skipped back from comes again.
    finishTrack fake
    es'' <- eventually fake (\xs -> length (startsOf xs) >= 4)
    take 1 (drop 3 (startsOf es'')) `shouldBe` [second]
    stopMusic player

  it "plays the album's first track again from its start on a step back" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playAlbum player album
    es <- eventually fake (not . null . startsOf)
    previousTrack player
    es' <- eventually fake (\xs -> length (startsOf xs) >= 2)
    take 2 (startsOf es') `shouldBe` replicate 2 (mconcat (take 1 (startsOf es)))
    stopMusic player

  it "holds the music, starts a track held while it is, and goes on when resumed" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playAlbum player album
    es <- eventually fake (not . null . startsOf)
    let first = mconcat (take 1 (startsOf es))
    pauseMusic player
    musicPaused player >>= (`shouldBe` True)
    es' <- eventually fake (elem (Paused first))
    Paused first `elem` es' `shouldBe` True
    -- Skipped to while held: the next one starts held, and is not heard
    -- (so not playing) before the resume.
    skipTrack player
    es'' <- eventually fake (\xs -> length (startsOf xs) >= 2)
    let second = mconcat (take 1 (drop 1 (startsOf es'')))
    es3 <- eventually fake (elem (Paused second))
    Paused second `elem` es3 `shouldBe` True
    threadDelay 20000
    nowPlaying player >>= (`shouldBe` Nothing)
    resumeMusic player
    musicPaused player >>= (`shouldBe` False)
    es4 <- eventually fake (elem (Resumed second))
    Resumed second `elem` es4 `shouldBe` True
    playingNow <- nowPlaying player
    fmap soundUrl playingNow `shouldBe` Just second
    stopMusic player

  it "goes two tracks back at once, then on through both" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playAlbum player album
    -- Three tracks: two played out, the third playing.
    _ <- playTracks fake 2
    es <- eventually fake (\xs -> length (startsOf xs) >= 3)
    let a = mconcat (take 1 (startsOf es))
        b = mconcat (take 1 (drop 1 (startsOf es)))
        c = mconcat (take 1 (drop 2 (startsOf es)))
    previousTrack player
    previousTrack player
    es' <- eventually fake (\xs -> lastOf (startsOf xs) == Just a)
    lastOf (startsOf es') `shouldBe` Just a
    finishTrack fake
    es'' <- eventually fake (\xs -> lastOf (startsOf xs) == Just b)
    lastOf (startsOf es'') `shouldBe` Just b
    finishTrack fake
    es3 <- eventually fake (\xs -> lastOf (startsOf xs) == Just c)
    lastOf (startsOf es3) `shouldBe` Just c
    stopMusic player

  it "lets the file playing go only after its voice has stopped, on a step back" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config {bufferAhead = 0, cacheSize = 0}
    playAlbum player album
    _ <- playTracks fake 1
    es <- eventually fake (\xs -> length (startsOf xs) >= 2)
    let second = mconcat (take 1 (drop 1 (startsOf es)))
    previousTrack player
    es' <- eventually fake (\xs -> length (startsOf xs) >= 3 && Stopped second `elem` xs)
    let at x = lookup x (zip es' [0 :: Int ..])
    case at (Released second) of
      Nothing -> pure ()
      Just r -> (at (Stopped second) < Just r) `shouldBe` True
    stopMusic player

  it "tells how far the music's track has played" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    musicProgress player >>= (`shouldBe` Nothing)
    playAlbum player album
    _ <- eventually fake (not . null . startsOf)
    musicProgress player >>= (`shouldBe` Just (1, 60))
    stopMusic player
    musicProgress player >>= (`shouldBe` Nothing)

  it "stops the music at once, and plays nothing more" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playAlbum player album
    es <- eventually fake (not . null . startsOf)
    let playing = mconcat (take 1 (startsOf es))
    stopMusic player
    _ <- eventually fake (Stopped playing `elem`)
    threadDelay 50000
    es' <- readIORef fake.events
    startsOf es' `shouldBe` [playing]
    Stopped playing `elem` es' `shouldBe` True

  it "replaces an album with a theme, which loops" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playAlbum player album
    es <- eventually fake (not . null . startsOf)
    let playing = mconcat (take 1 (startsOf es))
    playTheme player Menu
    es' <- eventually fake (Started "Menu" True `elem`)
    Stopped playing `elem` es' `shouldBe` True
    -- The album's voice stopped before the theme began.
    let after = dropWhile (/= Stopped playing) es'
    Started "Menu" True `elem` after `shouldBe` True
    stopMusic player

  it "plays an effect over the music" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playTheme player Menu
    _ <- eventually fake (Started "Menu" True `elem`)
    playEffect player Click
    es <- eventually fake (Started "Click" False `elem`)
    Started "Click" False `elem` es `shouldBe` True
    Stopped "Menu" `elem` es `shouldBe` False
    stopMusic player

  it "plays an album of a type that is not an enumeration" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    _ <- eventually fake (Fetched "Title" `elem`)
    playAlbum player (map Track [1 .. 3])
    starts <- playTracks fake 3
    sort starts `shouldBe` ["track-1", "track-2", "track-3"]
    stopMusic player

  it "while muted fetches and plays nothing, then brings the music back" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config {startMuted = True}
    playTheme player Menu
    playEffect player Click
    threadDelay 50000
    readIORef fake.events >>= (`shouldBe` [])
    setMuted player False
    es <- eventually fake (Started "Menu" True `elem`)
    Started "Menu" True `elem` es `shouldBe` True
    setMuted player True
    es' <- eventually fake (Stopped "Menu" `elem`)
    Stopped "Menu" `elem` es' `shouldBe` True

  it "changes the music's volume while it plays, and plays effects at theirs" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config {musicVolume = 0.5, effectsVolume = 0.8}
    playTheme player Menu
    _ <- eventually fake (Volume "Menu" 0.5 `elem`)
    setMusicVolume player 0.25
    es <- eventually fake (Volume "Menu" 0.25 `elem`)
    Volume "Menu" 0.25 `elem` es `shouldBe` True
    Stopped "Menu" `elem` es `shouldBe` False
    setEffectsVolume player 0.4
    playEffect player Click
    es' <- eventually fake (Volume "Click" 0.4 `elem`)
    Volume "Click" 0.4 `elem` es' `shouldBe` True
    -- The effects' volume leaves the music alone.
    [v | Volume "Menu" v <- es'] `shouldBe` [0.5, 0.25]
    stopMusic player

  it "at no music volume stops the music and fetches none, until it is raised" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playTheme player Menu
    _ <- eventually fake (Started "Menu" True `elem`)
    setMusicVolume player 0
    _ <- eventually fake (Stopped "Menu" `elem`)
    playAlbum player album
    threadDelay 50000
    es <- readIORef fake.events
    [t | Fetched t <- es, t `notElem` ["Menu", "Click"]] `shouldBe` []
    startsOf es `shouldBe` ["Menu"]
    setMusicVolume player 0.3
    -- A voice's volume is written down just after it starts: wait for it.
    es' <- eventually fake (\xs -> not (null [v | Volume t v <- xs, t /= "Menu"]))
    drop 1 (startsOf es') `shouldSatisfy` all (`elem` ["T1", "T2", "T3", "T4"])
    [v | Volume t v <- es', t /= "Menu"] `shouldBe` [0.3]
    stopMusic player

  it "plays no effect at no effects volume" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    _ <- eventually fake (Fetched "Click" `elem`)
    setEffectsVolume player 0
    playEffect player Click
    threadDelay 50000
    readIORef fake.events >>= (`shouldBe` []) . startsOf

  it "retires the music's voice before a volume change can reach it" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config
    playAlbum player album
    _ <- eventually fake (not . null . startsOf)
    reached <- newEmptyMVar
    release <- newEmptyMVar
    writeIORef fake.stopGate (Just (reached, release))
    -- The track ends; its voice is being stopped, and is held there.
    finishTrack fake
    timeout 2000000 (takeMVar reached) >>= (`shouldBe` Just ())
    changed <- newEmptyMVar
    _ <- forkIO (setMusicVolume player 0.2 >> putMVar changed ())
    threadDelay 20000
    writeIORef fake.stopGate Nothing
    putMVar release ()
    timeout 2000000 (takeMVar changed) >>= (`shouldBe` Just ())
    es <- readIORef fake.events
    [t | DeadVolume t <- es] `shouldBe` []
    stopMusic player

  it "starting at no music volume plays no music, and asks for no track, until it is raised" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config {musicVolume = 0}
    playTheme player T1
    es <- eventually fake (elem (Fetched "Menu"))
    startsOf es `shouldBe` []
    [t | Fetched t <- es, t `notElem` ["Menu", "Click"]] `shouldBe` []
    -- Persistent sounds are preloaded whatever the volume.
    Fetched "Menu" `elem` es `shouldBe` True
    setMusicVolume player 0.3
    es' <- eventually fake (Started "T1" True `elem`)
    Volume "T1" 0.3 `elem` es' `shouldBe` True
    stopMusic player

  it "unmuting at no music volume plays no music" $ do
    (fake, backend) <- newFake
    player <- newPlayer backend config {musicVolume = 0, startMuted = True}
    playAlbum player album
    setMuted player False
    threadDelay 50000
    es <- readIORef fake.events
    startsOf es `shouldBe` []
    [t | Fetched t <- es, t `notElem` ["Menu", "Click"]] `shouldBe` []
    playEffect player Click
    _ <- eventually fake (Started "Click" False `elem`)
    stopMusic player
