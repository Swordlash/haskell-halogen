{-# LANGUAGE TemplateHaskell #-}

-- | The panel, beside a counter: what it lists and counts, what it logs,
-- the state it puts back, the outline it draws, and that it leaves itself
-- out. The panel is mounted first, as a program opens it before what it
-- looks at.
module Test.Panel (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, replicateM_)
import Data.Text (Text)
import Data.Text qualified as T
import Example.Counter qualified as Counter
import Example.Forking qualified as Forking
import Halogen.DevTools qualified as DevTools
import Halogen.JSBits (JSText, Safety (..), browserJS, toJSText)
import Test.Hspec.Halogen

$( browserJS
     ["test/jsbits/hover.js"]
     [("js_hover", "halogen_devtools_test_hover", Unsafe, [t|JSText -> IO ()|])]
 )

-- | Move the mouse onto the first element matching the selector, as far as
-- its @mouseenter@ listeners can tell: the harness has no hover of its own.
hover :: Text -> PageM s ()
hover selector = unsafeIOToPageM (js_hover (toJSText selector)) >> settle

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "lists a counter mounted after it, and counts its renders, actions and changes" $ runPage $ do
    tools <- mount m DevTools.panel ()
    ui <- mount m Counter.component ()
    replicateM_ 2 (find ui "button.increment" >>= click)
    row <- find tools "[data-label=\"Counter\"]"
    findIn row ".halogen-devtools-name" >>= \name -> eventually (textContent name >>= (`shouldSatisfy` T.isPrefixOf "Counter#"))
    findIn row ".halogen-devtools-actions" >>= (`shouldHaveText` "2")
    findIn row ".halogen-devtools-changes" >>= (`shouldHaveText` "2")
    -- The first render, and one for each change.
    findIn row ".halogen-devtools-renders" >>= (`shouldHaveText` "3")

  it "logs the counter's events, newest first, and narrows the log to a kind" $ runPage $ do
    tools <- mount m DevTools.panel ()
    ui <- mount m Counter.component ()
    find ui "button.increment" >>= click
    eventually $ do
      whats <- logged tools
      take 2 whats `shouldBe` ["rendered", "state changed"]
      whats `shouldContain` ["action"]
      whats `shouldContain` ["mounted"]
    [button] <- filter' tools "states"
    click button
    eventually $ logged tools `shouldReturn` ["state changed"]

  it "puts an earlier state back, for a type it was told how to show" $ runPage $ do
    unsafeIOToPageM (DevTools.showing @Counter.State)
    tools <- mount m DevTools.panel ()
    ui <- mount m Counter.component ()
    replicateM_ 3 (find ui "button.increment" >>= click)
    eventually $ do
      states <- findAll tools ".halogen-devtools-state" >>= traverse textContent
      states `shouldBe` ["State {count = 3}", "State {count = 2}", "State {count = 1}"]
    restores <- findAll tools ".halogen-devtools-restore"
    click (last restores)
    find ui ".count" >>= (`shouldHaveText` "1")
    find tools ".halogen-devtools-status" >>= \status -> eventually (textContent status >>= (`shouldSatisfy` T.isPrefixOf "Put Counter#"))

  it "outlines the element of the component whose row is hovered" $ runPage $ do
    tools <- mount m DevTools.panel ()
    ui <- mount m Counter.component ()
    find ui "button.increment" >>= click
    _ <- find tools "[data-label=\"Counter\"]"
    hover ".halogen-devtools [data-label=\"Counter\"]"
    outline <- find tools ".halogen-devtools-outline"
    style <- getAttribute outline "style"
    style `shouldSatisfy` maybe False (\s -> "width: " `T.isInfixOf` s && not ("width: 0.0px" `T.isInfixOf` s))

  it "clears the log and the counts, what was still on its way to it included" $ runPage $ do
    tools <- mount m DevTools.panel ()
    ui <- mount m Counter.component ()
    find ui "button.increment" >>= click
    find tools "button.halogen-devtools-clear" >>= click
    -- The panel takes its events a tenth of a second after the first of a
    -- batch: whether the click's events had reached it before Clear or
    -- were still on their way, nothing of them is left afterwards.
    unsafeIOToPageM (threadDelay 300000)
    settle
    logged tools `shouldReturn` []
    row <- find tools "[data-label=\"Counter\"]"
    findIn row ".halogen-devtools-actions" >>= (`shouldHaveText` "0")
    findIn row ".halogen-devtools-renders" >>= (`shouldHaveText` "0")

  it "keeps a component that went away out of the tree, whatever it reports after" $ runPage $ do
    tools <- mount m DevTools.panel ()
    ui <- mount m Forking.component ()
    _ <- find tools "[data-label=\"Worker\"]"
    -- Taking the child out cancels its fork, whose end is reported after
    -- the child's finalization.
    find ui "button.remove" >>= click
    eventually $ (length <$> findAll tools "[data-label=\"Worker\"]") `shouldReturn` 0
    unsafeIOToPageM (threadDelay 300000)
    settle
    (length <$> findAll tools "[data-label=\"Worker\"]") `shouldReturn` 0
    whats <- findAll tools ".halogen-devtools-what" >>= traverse textContent
    whats `shouldSatisfy` any (\t -> "fork" `T.isPrefixOf` t && "ended" `T.isSuffixOf` t)

  it "leaves its own tree out of what it shows" $ runPage $ do
    tools <- mount m DevTools.panel ()
    ui <- mount m Counter.component ()
    find ui "button.increment" >>= click
    _ <- find tools "[data-label=\"Counter\"]"
    -- Its own renders (it has rendered the counter's row) are not there.
    labels <- findAll tools ".halogen-devtools-component" >>= traverse (`getAttribute` "data-label")
    labels `shouldBe` [Just "Counter"]
    whos <- findAll tools ".halogen-devtools-who" >>= traverse textContent
    filter ("DevTools" `T.isPrefixOf`) whos `shouldBe` []
  where
    -- What the log shows happened, newest first, each as its first word or
    -- two.
    logged tools = do
      entries <- findAll tools ".halogen-devtools-what"
      forM entries $ \e -> T.unwords . take 2 . T.words . T.replace " in " " " <$> textContent e >>= \t -> pure (if "rendered" `T.isPrefixOf` t then "rendered" else t)
    filter' tools name = do
      buttons <- findAll tools ".halogen-devtools-kind"
      named <- forM buttons $ \b -> (,b) <$> textContent b
      pure [b | (t, b) <- named, t == name]
