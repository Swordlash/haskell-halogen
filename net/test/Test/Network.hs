{-# LANGUAGE TemplateHaskell #-}

-- | The suite's view of the network double, test/web/network-double.js:
-- the requests it holds, by the name in their address, and the fixture the
-- runner serves.
module Test.Network
  ( started
  , aborted
  , release
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

-- | What test/web/fixture.txt holds.
fixtureText :: Text
fixtureText = "A file the browser suite fetches: zażółć gęślą jaźń.\n"
