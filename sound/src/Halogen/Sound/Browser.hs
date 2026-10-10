{-# LANGUAGE TemplateHaskell #-}

-- | The backend for a page: HTML audio elements, playing files fetched
-- whole into blob URLs, so that a file once fetched plays at once and is
-- never asked of the network again while it is held.
--
-- A fetch goes at low priority, so that sound does not hold up whatever the
-- page needs. A page may not play sound before its first click or key: a
-- voice started earlier waits for one, and then plays.
--
-- Natively there is nothing to play: the backend there fetches nothing and
-- its voices never end.
module Halogen.Sound.Browser
  ( Clip
  , Voice
  , browser
  )
where

import Halogen.JSBits
import Halogen.Sound.Backend
import Protolude

-- Natively nothing is fetched (a fetch never calls back) and a voice never
-- ends. 'fetchClip' does not wait there.
$(browserJS ["jsbits/sound.js"]
  [ ("js_fetch", "halogen_sound_fetch", Unsafe, [t| JSText -> Callback -> IO () |])
  , ("js_release", "halogen_sound_release", Unsafe, [t| JSVal -> IO () |])
  , ("js_start", "halogen_sound_start", Unsafe, [t| JSVal -> Double -> Bool -> Bool -> Double -> Callback -> Callback -> IO JSVal |])
  , ("js_stop", "halogen_sound_stop", Unsafe, [t| JSVal -> IO () |])
  , ("js_volume", "halogen_sound_volume", Unsafe, [t| JSVal -> Double -> IO () |])
  , ("js_pause", "halogen_sound_pause", Unsafe, [t| JSVal -> IO () |])
  , ("js_resume", "halogen_sound_resume", Unsafe, [t| JSVal -> IO () |])
  , ("js_time", "halogen_sound_time", Unsafe, [t| JSVal -> IO Double |])
  , ("js_duration", "halogen_sound_duration", Unsafe, [t| JSVal -> IO Double |])
  ])

browser :: Backend Clip Voice
browser =
  Backend
    { fetchClip
    , releaseClip
    , startVoice
    , stopVoice
    , setVolume
    , pauseVoice
    , resumeVoice
    , voiceProgress
    }

-- | A blob URL.
newtype Clip = Clip JSVal

-- | An audio element, and the callbacks it calls when it is heard and at
-- its end.
data Voice = Voice JSVal Callback Callback

fetchClip :: Text -> IO (Maybe Clip)
fetchClip _ | not inBrowser = pure Nothing
fetchClip url = do
  result <- newEmptyMVar
  done <- mkCallback $ \value -> putMVar result (if isNull value then Nothing else Just (Clip value))
  js_fetch (toJSText url) done
  clip <- takeMVar result
  freeCallback done
  pure clip

releaseClip :: Clip -> IO ()
releaseClip (Clip url) = js_release url

startVoice :: Clip -> Voicing -> IO () -> IO () -> IO Voice
startVoice (Clip url) Voicing {volume, looping, held, offset} started ended = do
  heard <- mkCallback (const started)
  done <- mkCallback (const ended)
  audio <- js_start url volume looping held offset heard done
  pure (Voice audio heard done)

stopVoice :: Voice -> IO ()
stopVoice (Voice audio heard done) = do
  js_stop audio
  freeCallback heard
  freeCallback done

setVolume :: Voice -> Double -> IO ()
setVolume (Voice audio _ _) = js_volume audio

pauseVoice :: Voice -> IO ()
pauseVoice (Voice audio _ _) = js_pause audio

resumeVoice :: Voice -> IO ()
resumeVoice (Voice audio _ _) = js_resume audio

voiceProgress :: Voice -> IO (Maybe (Double, Double))
voiceProgress (Voice audio _ _) = do
  at <- js_time audio
  total <- js_duration audio
  pure (if isNaN total || isInfinite total || total <= 0 then Nothing else Just (at, total))

