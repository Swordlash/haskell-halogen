-- | What a player needs from whatever makes the sound.
--
-- The player ('Halogen.Sound') decides what plays when, what is fetched
-- ahead and what is let go; a backend only fetches a file, and starts and
-- stops one voice of it. "Halogen.Sound.Browser" is the backend for a page;
-- a test can hand in one that only writes down what it was asked.
module Halogen.Sound.Backend
  ( Backend (..)
  , Voicing (..)
  )
where

import Protolude

data Backend clip voice = Backend
  { fetchClip :: Text -> IO (Maybe clip)
  -- ^ Fetch a file, ready to play; 'Nothing' if it could not be had. May
  -- block for as long as the fetch takes (the player calls it on a thread of
  -- its own).
  , releaseClip :: clip -> IO ()
  -- ^ Let a fetched file go. It is not playing when this is called.
  , startVoice :: clip -> Voicing -> IO () -> IO () -> IO voice
  -- ^ Start playing a file. The first action is called when it is heard
  -- (maybe later than this returns: a page plays nothing before its first
  -- click), the second once if it comes to its end by itself (a looping
  -- voice never does). Neither after 'stopVoice'.
  , stopVoice :: voice -> IO ()
  -- ^ Stop a voice, whether or not it has ended already.
  , setVolume :: voice -> Double -> IO ()
  -- ^ Change a playing voice's volume, from 0 to 1.
  , pauseVoice :: voice -> IO ()
  -- ^ Hold a voice where it is; it does not end while held.
  , resumeVoice :: voice -> IO ()
  -- ^ Go on with a held voice from where it was held.
  , voiceProgress :: voice -> IO (Maybe (Double, Double))
  -- ^ How far a voice has played and how long its file is, in seconds;
  -- 'Nothing' while the length is not known.
  }

data Voicing = Voicing
  { volume :: Double
  -- ^ From 0 to 1.
  , looping :: Bool
  }
  deriving stock (Eq, Show)
