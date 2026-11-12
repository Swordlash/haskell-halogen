-- The hspec layer's program in the playground's test (../Playground.hs,
-- which says what it checks): an hspec-halogen spec run in the page, its
-- report on stdout.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Test.Hspec.Halogen
import Web.DOM.ParentNode (QuerySelector (..))

data Action = Clicked | Typed Text

tested :: (MonadIO m) => H.Component q () o m
tested =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure (0 :: Int, "" :: Text)
      , H.render = \(n, t) ->
          HH.div_
            [ HH.button [HE.onClick (const Clicked)] [HH.text ("clicked " <> T.pack (show n))]
            , HH.input [HE.onValueInput Typed]
            , HH.p_ [HH.text t]
            ]
      , H.eval = H.mkEval H.defaultEval {H.handleAction = handle}
      }
  where
    handle Clicked = modify (\(n, t) -> (n + 1, t))
    handle (Typed t) = modify (\(n, _) -> (n, t))

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = describe "in the page" $ do
  it "clicks" $ runPage $ do
    ui <- mount m tested ()
    button <- find ui "button"
    click button
    shouldHaveText button "clicked 1"
  it "types" $ runPage $ do
    ui <- mount m tested ()
    input <- find ui "input"
    typeText input "hello"
    paragraph <- find ui "p"
    shouldHaveText paragraph "hello"

main :: IO ()
main = do
  app <- runBrowserDOM $ do
    HA.endPreviousRun
    maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  runBrowserTestsIn app (spec BrowserDOM)
