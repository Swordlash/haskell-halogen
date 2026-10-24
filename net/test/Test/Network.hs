{-# LANGUAGE TemplateHaskell #-}

-- | The suite's view of the network double, test/web/network-double.js:
-- the requests it holds, by the name in their address, and the fixture the
-- runner serves.
module Test.Network
  ( started
  , aborted
  , release
  , reset
  , fixtureText
  )
where

import Data.Text (Text)
import Halogen.JSBits (JSText, Safety (..), browserJS, toJSText)

$( browserJS
     ["test/jsbits/network.js"]
     [ ("js_started", "halogen_net_test_started", Unsafe, [t|JSText -> IO Bool|])
     , ("js_aborted", "halogen_net_test_aborted", Unsafe, [t|JSText -> IO Bool|])
     , ("js_release", "halogen_net_test_release", Unsafe, [t|JSText -> IO ()|])
     , ("js_reset", "halogen_net_test_reset", Unsafe, [t|IO ()|])
     ]
 )

-- | Whether the request held under this name has reached the network.
started :: Text -> IO Bool
started = js_started . toJSText

-- | Whether it was aborted there, before it was let go.
aborted :: Text -> IO Bool
aborted = js_aborted . toJSText

-- | Let it go: a held request is then fetched, and a streamed body ends.
release :: Text -> IO ()
release = js_release . toJSText

-- | Forget every request the double has seen, letting go of any still held:
-- a run under browser GHCi finds the page as the run before left it, names
-- released and aborts noted. Natively it does nothing.
reset :: IO ()
reset = js_reset

-- | What test/web/fixture.txt holds.
fixtureText :: Text
fixtureText = "A file the browser suite fetches: zażółć gęślą jaźń.\n"
