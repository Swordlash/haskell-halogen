{-# LANGUAGE TemplateHaskell #-}

-- | What the suite reads of the scenes on the page (test/jsbits/scene.js).
module Test.Page
  ( moduleUrl
  , drawn
  , live
  , colorOf
  , watchDisposals
  , Disposed (..)
  , disposed
  )
where

import Halogen.JSBits (JSText, Safety (..), browserJS, toJSText)
import Halogen.JSBits.Promise (Promise (..), await)
import Protolude
import System.IO.Error (userError)
import Test.Hspec.Halogen (PageM, unsafeIOToPageM)

-- | Where test/web/bundle.sh puts Three.js, which the runner serves: the
-- suite needs no network.
moduleUrl :: Text
moduleUrl = "/three.mjs"

$( browserJS
     ["test/jsbits/scene.js"]
     [ ("js_drawn", "halogen_three_test_drawn", Unsafe, [t|IO Int|])
     , ("js_live", "halogen_three_test_live", Unsafe, [t|IO Int|])
     , ("js_color", "halogen_three_test_color", Unsafe, [t|JSText -> IO Int|])
     , ("js_watch_disposals", "halogen_three_test_watch_disposals", Unsafe, [t|JSText -> IO Promise|])
     , ("js_disposed_geometries", "halogen_three_test_disposed_geometries", Unsafe, [t|IO Int|])
     , ("js_disposed_materials", "halogen_three_test_disposed_materials", Unsafe, [t|IO Int|])
     ]
 )

-- | How many times the scenes on the page have been drawn.
drawn :: PageM s Int
drawn = unsafeIOToPageM js_drawn

-- | How many scenes on the page have a renderer.
live :: PageM s Int
live = unsafeIOToPageM js_live

-- | The colour of the named mesh's material, if there is such a mesh.
colorOf :: Text -> PageM s (Maybe Int)
colorOf name = unsafeIOToPageM $ do
  color <- js_color (toJSText name)
  pure (if color < 0 then Nothing else Just color)

-- | Count the geometries and materials disposed of from now on.
watchDisposals :: PageM s ()
watchDisposals = unsafeIOToPageM $ do
  outcome <- await =<< js_watch_disposals (toJSText moduleUrl)
  either (\problem -> throwIO (userError ("could not load Three.js: " <> show problem))) (const pass) outcome

data Disposed = Disposed {geometries :: Int, materials :: Int}
  deriving stock (Eq, Show)

disposed :: PageM s Disposed
disposed = unsafeIOToPageM (Disposed <$> js_disposed_geometries <*> js_disposed_materials)
