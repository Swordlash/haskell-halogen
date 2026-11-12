-- | Loading the compiler where it does not go as on a wide screen with a
-- good network: a narrow screen, a manifest that fails and then lacks the
-- version and the sizes, a layer that fails. Each in a context of its own,
-- whose Cache API is empty.
module Tutorial.Loading (spec) where

import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, stateTVar)
import Control.Monad (unless)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import GHC.Clock (getMonotonicTime)
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Tutorial.Site

-- | A page in a context of its own with these options, its watch expecting
-- the console errors the test causes, checked at the end.
inContext :: Site -> IORef [Text] -> ContextOptions -> (Text -> Bool) -> (Page -> Watch -> IO ()) -> Playwright -> IO ()
inContext site allowed options expected body pw =
  withContext pw options $ \ctx -> do
    page <- withTimeout 30000 <$> newPage ctx
    watch <- newWatch site ctx expected allowed
    withFailureContext page $ do
      body page watch
      checkWatch watch page

wide :: ContextOptions
wide = defaultContextOptions {viewport = Just (1400, 900), acceptDialogs = True}

-- | A manifest as an older build's: no version, and no sizes, of the
-- archives or of their parts, and an archive in one part as the file
-- itself (the first manifests had no parts).
bare :: Value -> Value
bare = \case
  Object o -> Object (KeyMap.map bare (dropParts (foldr KeyMap.delete o ["version", "size", "compressedSize"])))
  Array a -> Array (Vector.map bare a)
  other -> other
  where
    dropParts o = case (KeyMap.lookup "parts" o, KeyMap.lookup "archive" o) of
      (Just (Array ps), Just archive')
        | [Object part] <- Vector.toList ps
        , KeyMap.lookup "path" part == Just archive' ->
            KeyMap.delete "parts" o
      _ -> o

spec :: Site -> IORef [Text] -> SpecWith Playwright
spec site allowed = describe "loading the compiler" $ do
  -- A narrow screen downloads nothing large until asked: not even a HEAD,
  -- since the manifest has the archive's size, which the page shows. What
  -- the page fetches when it starts (the manifest, for the sizes) is the
  -- program's, so once it is quiet nothing more is coming.
  it "waits to be asked on a narrow screen, the code read-only until then" $
    inContext site allowed defaultContextOptions {viewport = Just (400, 800)} (const False) $ \page watch -> do
      visit (long site page) (site.url <> "01-hello/")
      entered watch page
      waitQuiet (long site page)
      readOnce (locator page "button.learn-load") "the button that loads the compiler"
      asked <- requests (pageContext page)
      let archives = [r.method <> " " <> r.url | r <- asked, archive r]
      unless (null archives) $ expectationFailure ("archives downloaded, or asked about, on a narrow screen: " <> show archives)
      count (locator page ".cm-content[contenteditable=\"false\"]") `shouldReturn` 1
      report "on a narrow screen the compiler waits to be asked, and the code is read-only"
      since <- getMonotonicTime
      click (locator page "button.learn-load")
      untilReady site page since ""
      count (locator page ".cm-content[contenteditable=\"true\"]") `shouldReturn` 1
      report "loading it anyway makes the code editable"

  -- A compiler that does not load says why, and Retry tries again. The
  -- second time, the manifest is one without a version or sizes (as an
  -- older build's), and the page asks the server for them instead.
  it "says why a compiler did not load, and Retry loads it from a manifest without a version or sizes" $
    inContext site allowed wide (\t -> "rootfs.json" `Text.isInfixOf` t || "503" `Text.isInfixOf` t) $ \page watch -> do
      refused <- newTVarIO (0 :: Int)
      let stripped = LazyByteString.toStrict (Aeson.encode (bare site.manifestJSON))
      _ <- route page "**/rootfs/rootfs.json" $ \_ -> do
        n <- atomically (stateTVar refused (\k -> (k, k + 1)))
        pure $
          if n == 0
            then fulfill 503 Nothing ""
            else fulfill 200 (Just "application/json") stripped
      visit (long site page) (site.url <> "01-hello/")
      entered watch page
      waitQuiet (long site page)
      readOnce (locator page "button.learn-retry") "Retry"
      contains (locator page ".learn-compiler .learn-error") "rootfs.json: 503"
      since <- getMonotonicTime
      before <- length <$> requests (pageContext page)
      click (locator page "button.learn-retry")
      untilReady site page since ""
      asked <- drop before <$> requests (pageContext page)
      let head' = site.url <> "rootfs/" <> site.manifest.firstPart
      unless (any (\r -> r.method == "HEAD" && r.url == head') asked) $
        expectationFailure (Text.unpack ("without a version in the manifest, the page does not ask the server for the archive's: no HEAD " <> head'))
      report "a compiler that fails to load says why, and Retry loads it, from a manifest without a version or sizes"

  -- A layer that does not load is its lessons' failure, with a Retry of
  -- its own: the compiler is ready, and a lesson without the layer
  -- compiles.
  it "makes a layer that does not load its lessons' failure alone, and loads it when tried again" $ \pw ->
    case [(lesson, name) | lesson <- site.lessons, name : _ <- [lesson.layers]] of
      [] -> pendingWith "no lesson has a layer"
      (layered, name) : _ -> inContext site allowed wide ("503" `Text.isInfixOf`) (layerFails layered name) pw
  where
    layerFails layered name page watch = do
      refusals <- newTVarIO (0 :: Int)
      _ <- route page ("**/rootfs/layers/" <> name <> ".tar.zst*") $ \_ -> do
        n <- atomically (stateTVar refusals (\k -> (k, k + 1)))
        pure (if n < 2 then fulfill 503 Nothing "" else Continue)
      since <- getMonotonicTime
      visit (long site page) (site.url <> layered.slug <> "/")
      entered watch page
      untilReady site page since ""
      readOnce (locator page "button.learn-retry-layers") "the layers' Retry"
      contains (locator page ".learn-result-column .learn-error") "503"
      click (locator page "button.learn-retry-layers")
      waitQuiet (long site page)
      tried <- readTVarIO refusals
      unless (tried >= 2) $ expectationFailure "Retry does not fetch the layer again"
      readOnce (locator page "button.learn-retry-layers") "the layers' Retry, after the second refusal"
      mapM_ (openLesson site watch page) (take 1 site.lessons)
      compiled site page
      outcome page >>= \said -> unless ("Compiled and ran" `Text.isPrefixOf` said) $ expectationFailure (Text.unpack ("a lesson without the layer compiles: " <> said))
      openLesson site watch page layered
      count (locator page "button.learn-retry-layers") `shouldReturn` 0
      report ("a layer (" <> name <> ") that fails to load is " <> layered.slug <> "'s failure alone, and loads when tried again")
