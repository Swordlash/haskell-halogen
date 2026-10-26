-- | A scene in a real page: Three.js loaded, WebGL (SwiftShader, in
-- headless Chromium), and the pointer as Playwright moves it.
module Test.Browser (spec) where

import Control.Monad.UUID (MonadUUID)
import Example.Scene qualified as Scene
import Halogen qualified as H
import Halogen.Three qualified as Three
import Protolude hiding (find)
import Test.Hspec.Halogen
import Test.Page qualified as Page

spec :: forall m -> (MonadBrowserTest m, MonadUUID m) => Spec
spec m = do
  it "puts a canvas on the page and draws the scene on it" $ runPage $ do
    ui <- mountScene m
    _ <- find ui "canvas"
    Page.live `shouldReturn` 1
    eventually (Page.colorOf Scene.boxName `shouldReturn` Just Scene.firstColor)

  it "hands a click on the box to its handler, past a speck the camera clips, and the parent hears it" $ runPage $ do
    ui <- mountScene m
    find ui "canvas" >>= click
    find ui ".heard" >>= (`shouldHaveText` "clicks: 1, moves: 0")
    query ui (H.mkRequest Scene.GetHeard) `shouldReturn` Just (Scene.Heard 1 0)

  it "turns the camera with a drag, raises the camera once, and makes no click of it" $ runPage $ do
    ui <- mountScene m
    canvas <- find ui "canvas"
    find ui ".elsewhere" >>= dragTo canvas
    find ui ".heard" >>= (`shouldHaveText` "clicks: 0, moves: 1")
    -- The camera turned about its target, where the box is, so the box is
    -- still at the canvas's centre, and the next click is a click again.
    click canvas
    find ui ".heard" >>= (`shouldHaveText` "clicks: 1, moves: 1")

  it "patches the material when the colour changes" $ runPage $ do
    ui <- mountScene m
    find ui ".recolour" >>= click
    eventually (Page.colorOf Scene.boxName `shouldReturn` Just Scene.secondColor)

  it "disposes of a removed mesh's geometry and material" $ runPage $ do
    ui <- mountScene m
    Page.watchDisposals
    before <- Page.disposed
    find ui ".remove" >>= click
    eventually (Page.colorOf Scene.boxName `shouldReturn` Nothing)
    Page.disposed `shouldReturn` Page.Disposed {geometries = before.geometries + 1, materials = before.materials + 1}

  it "lets go of the renderer and what is left of the scene when it goes" $ runPage $ do
    ui <- mountScene m
    Page.watchDisposals
    before <- Page.disposed
    unmount ui
    eventually (Page.live `shouldReturn` 0)
    after <- Page.disposed
    -- The box, and the placeholders a mesh has before its props say.
    after.geometries `shouldSatisfy` (>= before.geometries + 2)
    after.materials `shouldSatisfy` (>= before.materials + 2)

-- | The scene, with Three.js from the runner's server, once it has been
-- drawn: until Three.js has loaded there is nothing to click.
mountScene :: forall m -> (MonadBrowserTest m, MonadUUID m) => PageM s (Mounted s Scene.Query o m)
mountScene m = do
  ui <- mount m (Scene.component (Three.Config {moduleUrl = Page.moduleUrl})) ()
  eventuallyWithin 30000 (Page.drawn >>= (`shouldSatisfy` (> 0)))
  pure ui
