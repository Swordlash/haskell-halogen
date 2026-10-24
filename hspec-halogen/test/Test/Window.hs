{-# LANGUAGE TemplateHaskell #-}

module Test.Window (spec) where

import Data.IORef (IORef, newIORef, readIORef)
import Example.Window qualified as Window
import Halogen qualified as H
import Halogen.JSBits (JSText, Safety (..), browserJS, toJSText)
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, window)
import Protolude hiding (find)
import Test.Hspec.Halogen
import Web.HTML.Window (innerHeight, innerWidth)

-- The page's events are dispatched by test/jsbits/window.js, declared once
-- for both browser backends.
$( browserJS
     ["test/jsbits/window.js"]
     [ ("js_dispatch", "halogen_window_dispatch", Unsafe, [t|JSText -> IO ()|])
     , ("js_set_visibility", "halogen_window_set_visibility", Unsafe, [t|JSText -> IO ()|])
     , ("js_restore_visibility", "halogen_window_restore_visibility", Unsafe, [t|IO ()|])
     ]
 )

spec :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => Spec
spec m = do
  it "hears a key pressed on the page" $ runPage $ do
    (_, ui) <- mountIt m
    press "a"
    press "Enter"
    find ui ".keys" >>= (`shouldHaveText` "a Enter")

  it "measures the window as it is resized" $ runPage $ do
    (_, ui) <- mountIt m
    (w, h) <- unsafeIOToPageM $ runTest @m $ do
      win <- window
      (,) <$> innerWidth win <*> innerHeight win
    dispatch "resize"
    find ui ".sizes" >>= (`shouldHaveText` (show w <> "x" <> show h))

  it "reads whether the page can be seen each time that changes" $ runPage $ do
    (_, ui) <- mountIt m
    setVisibility "hidden"
    find ui ".visibility" >>= (`shouldHaveText` "hidden")
    setVisibility "visible"
    unsafeIOToPageM js_restore_visibility
    find ui ".visibility" >>= (`shouldHaveText` "hidden visible")

  it "hears the browser go offline and online" $ runPage $ do
    (_, ui) <- mountIt m
    dispatch "offline"
    dispatch "online"
    find ui ".online" >>= (`shouldHaveText` "offline online")

  it "hears the screen's frames, each later than the last" $ runPage $ do
    (_, ui) <- mountIt m
    eventually $ do
      frames <- fromMaybe [] <$> query ui (H.mkRequest Window.GetFrames)
      frames `shouldSatisfy` ((>= 3) . length)
      and (zipWith (<) frames (drop 1 frames)) `shouldBe` True

  it "ticks on an interval" $ runPage $ do
    (_, ui) <- mountIt m
    ticks <- find ui ".ticks"
    eventually $ do
      shown <- textContent ticks
      (readMaybe shown :: Maybe Int) `shouldSatisfy` maybe False (>= 3)

  it "hears nothing more once it has unsubscribed" $ runPage $ do
    (counter, ui) <- mountIt m
    started ui
    void $ query ui (H.mkTell Window.Stop)
    nothingMore counter
    -- The component is still there, and heard none of it.
    find ui ".keys" >>= (`shouldHaveText` "")
    find ui ".online" >>= (`shouldHaveText` "")

  it "hears nothing more once it is gone" $ runPage $ do
    (counter, ui) <- mountIt m
    started ui
    unmount ui
    nothingMore counter

-- | The component, with a counter of what its emitters deliver.
mountIt :: forall m -> (MonadBrowserTest m, MonadBrowserDOM m) => PageM s (IORef Int, Mounted s Window.Query Void m)
mountIt m = do
  counter <- unsafeIOToPageM (newIORef 0)
  ui <- mount m (Window.component :: H.Component Window.Query (IORef Int) Void m) counter
  pure (counter, ui)

-- | Wait for both clocks to have started, so that what stops them is seen
-- to.
started :: (MonadBrowserTest m) => Mounted s Window.Query Void m -> PageM s ()
started ui = do
  eventually $ do
    frames <- fromMaybe [] <$> query ui (H.mkRequest Window.GetFrames)
    frames `shouldSatisfy` (not . null)
  ticks <- find ui ".ticks"
  eventually (textContent ticks >>= (`shouldNotBe` "0"))

-- | Make everything the emitters hear happen, and wait long enough for
-- several frames and ticks: none of it may be delivered. A frame or a tick
-- already on its way as the subscriptions ended may still arrive, so the
-- count is taken a little after.
nothingMore :: IORef Int -> PageM s ()
nothingMore counter = do
  unsafeIOToPageM (threadDelay 50000)
  before <- unsafeIOToPageM (readIORef counter)
  press "a"
  dispatch "resize"
  dispatch "offline"
  dispatch "online"
  setVisibility "hidden"
  unsafeIOToPageM js_restore_visibility
  unsafeIOToPageM (threadDelay 200000)
  unsafeIOToPageM (readIORef counter) `shouldReturn` before

dispatch :: Text -> PageM s ()
dispatch ty = unsafeIOToPageM (js_dispatch (toJSText ty)) >> settle

setVisibility :: Text -> PageM s ()
setVisibility seen = unsafeIOToPageM (js_set_visibility (toJSText seen)) >> settle
