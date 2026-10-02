{-# LANGUAGE CPP #-}

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

import Halogen.Sound.Backend
import Protolude

#if defined(javascript_HOST_ARCH)
import GHC.JS.Foreign.Callback qualified as JS
import GHC.JS.Prim (JSVal, isNull, toJSString)
#elif defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString (..), JSVal, freeJSVal, toJSString)
#endif

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

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

-- | A blob URL.
newtype Clip = Clip JSVal

-- | An audio element, and the callbacks it calls when it is heard and at
-- its end.
data Voice = Voice JSVal Callback Callback

fetchClip :: Text -> IO (Maybe Clip)
fetchClip url = do
  result <- newEmptyMVar
  done <- mkCallback $ \value -> putMVar result (if jsIsNull value then Nothing else Just (Clip value))
  js_fetch (jsText url) done
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

#endif

#if defined(javascript_HOST_ARCH)

type Callback = JS.Callback (JSVal -> IO ())

mkCallback :: (JSVal -> IO ()) -> IO Callback
mkCallback = JS.asyncCallback1

freeCallback :: Callback -> IO ()
freeCallback = JS.releaseCallback

jsText :: Text -> JSVal
jsText = toJSString . toS

jsIsNull :: JSVal -> Bool
jsIsNull = isNull

foreign import javascript unsafe "halogen_sound_fetch" js_fetch :: JSVal -> Callback -> IO ()
foreign import javascript unsafe "halogen_sound_release" js_release :: JSVal -> IO ()
foreign import javascript unsafe "halogen_sound_start" js_start :: JSVal -> Double -> Bool -> Bool -> Double -> Callback -> Callback -> IO JSVal
foreign import javascript unsafe "halogen_sound_stop" js_stop :: JSVal -> IO ()
foreign import javascript unsafe "halogen_sound_volume" js_volume :: JSVal -> Double -> IO ()
foreign import javascript unsafe "halogen_sound_pause" js_pause :: JSVal -> IO ()
foreign import javascript unsafe "halogen_sound_resume" js_resume :: JSVal -> IO ()
foreign import javascript unsafe "((a) => a.currentTime)" js_time :: JSVal -> IO Double
foreign import javascript unsafe "((a) => a.duration)" js_duration :: JSVal -> IO Double

#elif defined(wasm32_HOST_ARCH)

type Callback = JSVal

foreign import javascript "wrapper" mkCallback :: (JSVal -> IO ()) -> IO Callback

freeCallback :: Callback -> IO ()
freeCallback = freeJSVal

jsText :: Text -> JSString
jsText = toJSString . toS

foreign import javascript unsafe "$1 === null" jsIsNull :: JSVal -> Bool
foreign import javascript unsafe "fetch($1, {priority: 'low'}).then(r => r.ok ? r.blob() : null).then(b => $2(b ? URL.createObjectURL(b) : null)).catch(() => $2(null))" js_fetch :: JSString -> Callback -> IO ()
foreign import javascript unsafe "URL.revokeObjectURL($1)" js_release :: JSVal -> IO ()
-- Before the page's first click or key a browser refuses to play; the
-- voice then waits for one (unless it has been stopped by then), by one
-- listener pair however often it is refused. A voice started held plays
-- nothing until resumed, and a file failing while held ends it once
-- resumed. The same as jsbits/sound.js.
foreign import javascript unsafe "const a = new Audio($1); a.volume = $2; a.loop = $3; a.__halogenPaused = $4; const fail = () => { if (a.__halogenStopped) return; if (a.__halogenPaused) { a.__halogenFailed = true; return; } $7(null); }; a.onplaying = () => $6(null); a.onended = () => $7(null); a.onerror = fail; let seek = Number.isFinite($5) ? Math.max(0, $5) : 0; const place = () => { if (a.__halogenStopped || a.readyState < 1) return; if (seek > 0) { a.currentTime = Math.min(seek, Number.isFinite(a.duration) ? Math.max(0, a.duration - 0.001) : seek); seek = 0; } }; a.onloadedmetadata = () => { place(); go(); }; const go = () => { if (a.__halogenStopped || a.__halogenPaused) return; if (seek > 0 && a.readyState < 1) return; place(); a.play().catch((e) => { if (a.__halogenStopped || a.__halogenPaused) return; if (!e || e.name !== 'NotAllowedError') { fail(); return; } if (a.__halogenWaiting) return; a.__halogenWaiting = true; const retry = () => { removeEventListener('pointerdown', retry, true); removeEventListener('keydown', retry, true); a.__halogenWaiting = false; go(); }; addEventListener('pointerdown', retry, true); addEventListener('keydown', retry, true); }); }; a.__halogenGo = go; a.__halogenFail = fail; go(); return a;" js_start :: JSVal -> Double -> Bool -> Bool -> Double -> Callback -> Callback -> IO JSVal
foreign import javascript unsafe "$1.__halogenStopped = true; $1.onplaying = null; $1.onended = null; $1.onerror = null; $1.onloadedmetadata = null; $1.pause(); $1.removeAttribute('src'); $1.load();" js_stop :: JSVal -> IO ()
foreign import javascript unsafe "$1.volume = Math.min(1, Math.max(0, $2));" js_volume :: JSVal -> Double -> IO ()
-- A resume starts the voice as a start does (a refusal waited out, a file
-- failed while held ending it).
foreign import javascript unsafe "$1.__halogenPaused = true; $1.pause();" js_pause :: JSVal -> IO ()
foreign import javascript unsafe "$1.__halogenPaused = false; if (!$1.__halogenStopped) { if ($1.__halogenFailed) { $1.__halogenFailed = false; $1.__halogenFail(); } else $1.__halogenGo(); }" js_resume :: JSVal -> IO ()
foreign import javascript unsafe "$1.currentTime" js_time :: JSVal -> IO Double
foreign import javascript unsafe "$1.duration" js_duration :: JSVal -> IO Double

#else

-- | Never had: nothing is fetched here.
data Clip

data Voice = Voice

fetchClip :: Text -> IO (Maybe Clip)
fetchClip _ = pure Nothing

releaseClip :: Clip -> IO ()
releaseClip _ = pass

startVoice :: Clip -> Voicing -> IO () -> IO () -> IO Voice
startVoice _ _ _ _ = pure Voice

stopVoice :: Voice -> IO ()
stopVoice _ = pass

setVolume :: Voice -> Double -> IO ()
setVolume _ _ = pass

pauseVoice :: Voice -> IO ()
pauseVoice _ = pass

resumeVoice :: Voice -> IO ()
resumeVoice _ = pass

voiceProgress :: Voice -> IO (Maybe (Double, Double))
voiceProgress _ = pure Nothing

#endif
