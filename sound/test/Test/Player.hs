-- | The player, over a backend that plays nothing and writes down what it
-- is asked: the test ends the tracks itself.
module Test.Player (spec) where

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Halogen.Sound
import Prelude
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

data Event = Fetched Text | Released Text | Started Text Bool | Stopped Text
  deriving stock (Eq, Show)

data Fake = Fake
  { events :: IORef [Event]
  , voices :: IORef (Map.Map Int (Text, IO ()))
  , counter :: IORef Int
  }

newFake :: IO (Fake, Backend Text Int)
newFake = do
  fake <- Fake <$> newIORef [] <*> newIORef mempty <*> newIORef 0
  let note e = atomicModifyIORef' fake.events (\es -> (es <> [e], ()))
      backend =
        Backend
          { fetchClip = \url -> threadDelay 1000 >> note (Fetched url) >> pure (Just url)
          , releaseClip = note . Released
          , startVoice = \clip Voicing {looping} ended -> do
              n <- atomicModifyIORef' fake.counter (\i -> (i + 1, i))
              atomicModifyIORef' fake.voices (\vs -> (Map.insert n (clip, ended) vs, ()))
              note (Started clip looping)
              pure n
          , stopVoice = \n -> do
              v <- atomicModifyIORef' fake.voices (\vs -> (Map.delete n vs, Map.lookup n vs))
              mapM_ (note . Stopped . fst) v
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
