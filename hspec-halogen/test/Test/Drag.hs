module Test.Drag (spec) where

import Data.Map.Strict qualified as Map
import Example.Drag qualified as Drag
import Halogen qualified as H
import Protolude hiding (find)
import Test.Hspec.Halogen

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  describe "HTML5 drag and drop" (sorting m Drag.Html5)
  describe "pointer events" (sorting m Drag.Pointer)

sorting :: forall m -> (MonadBrowserTest m) => Drag.Kind -> Spec
sorting m kind = do
  it "drags a piece into a bin" $ runPage $ do
    ui <- mount m Drag.component kind
    piece <- find ui ".piece-b"
    find ui ".bin-right" >>= dragTo piece
    query ui (H.mkRequest Drag.GetBins) `shouldReturn` Just (Map.fromList [("b", "right")])
    find ui ".bin-right" >>= (`shouldHaveText` "right: b")
    length <$> findAll ui ".tray .piece" `shouldReturn` 2

  it "drags one piece after another" $ runPage $ do
    ui <- mount m Drag.component kind
    find ui ".piece-a" >>= \piece -> find ui ".bin-left" >>= dragTo piece
    find ui ".piece-c" >>= \piece -> find ui ".bin-left" >>= dragTo piece
    find ui ".bin-left" >>= (`shouldHaveText` "left: ac")
