{-# LANGUAGE CPP #-}

-- | "Halogen.Static": a component rendered to HTML with no browser, after
-- it has initialized, and disposed of once the HTML is taken.
module Test.Static (spec) where

import Prelude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "static rendering" $ pure ()

#else

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Class (put)
import Data.IORef
import Data.Row (Empty, type (.==))
import Data.Text (Text)
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.IO.Util (readEmbedded, selectElement)
import Halogen.Static (embed, fillMountPoint, renderToHtml, renderToHtmlWith)
import Halogen.Subscription qualified as HS
import Data.MediaType (MediaType (..))
import Halogen.VDom.Driver (runUI)
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Web.DOM.ParentNode (QuerySelector (..))
import Test.Hspec (Spec, describe, it)
import Test.Utils (assertEqual, assertWith)

-- | What it renders is what its initializer read, through 'liftIO', from
-- the cell it is given.
initialized :: IORef Text -> H.Component H.VoidF () Void MemDOM
initialized source =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure "before"
      , render = \st -> HH.p_ [HH.text st] :: H.ComponentHTML () Empty MemDOM
      , eval = H.mkEval H.defaultEval {H.handleAction = \() -> liftIO (readIORef source) >>= put, H.initialize = Just ()}
      }

child :: H.Component H.VoidF () Void MemDOM
child =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ -> HH.span_ [HH.text "the child"] :: H.ComponentHTML () Empty MemDOM
      , eval = H.mkEval H.defaultEval
      }

parent :: H.Component H.VoidF () Void MemDOM
parent =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ ->
          HH.div [HP.class_ (HH.ClassName "parent")] [HH.slot_ "child" () child ()]
            :: H.ComponentHTML () ("child" .== H.Slot H.VoidF Void ()) MemDOM
      , eval = H.mkEval H.defaultEval
      }

data Action = Start | Tick

-- | Subscribes when it starts, to an emitter that records when it is
-- unsubscribed from, and records its own finalizer.
subscribing :: IORef [Text] -> H.Component H.VoidF () Void MemDOM
subscribing events =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ -> HH.text "subscribed" :: H.ComponentHTML Action Empty MemDOM
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = \case
                  Start -> do
                    let emitter = HS.Emitter $ \_ -> pure (HS.Subscription (liftIO (record "unsubscribed")))
                    void (H.subscribe emitter)
                  -- At once rather than through liftIO: disposing of a
                  -- tree runs its finalizers, but does not wait for what
                  -- they wait for.
                  Tick -> H.liftEffect (liftIO (record "finalized"))
              , H.initialize = Just Start
              , H.finalize = Just Tick
              }
      }
  where
    record e = modifyIORef' events (<> [e])

-- | What 'embed' writes, as a component would build it: a script with an id,
-- holding the text.
embedded :: Text -> Text -> H.Component H.VoidF () Void MemDOM
embedded ident json =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ ->
          HH.script [HP.id ident, HP.type_ (MediaType "application/json")] [HH.text json]
            :: H.ComponentHTML () Empty MemDOM
      , eval = H.mkEval H.defaultEval
      }

-- | An application monad over the in-memory one: what it renders comes from
-- its environment.
fromConfig :: H.Component H.VoidF () Void (ReaderT Text MemDOM)
fromConfig =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> ask
      , render = \st -> HH.p_ [HH.text st] :: H.ComponentHTML () Empty (ReaderT Text MemDOM)
      , eval = H.mkEval H.defaultEval
      }

spec :: Spec
spec = describe "static rendering" $ do
  it "renders what the component's initializer left" $ do
    source <- newIORef "after"
    renderToHtml (initialized source) () >>= assertEqual "markup" "<p>after</p>"

  it "renders a child in its slot" $
    renderToHtml parent () >>= assertEqual "markup" "<div class=\"parent\"><span>the child</span></div>"

  it "disposes of the tree once the HTML is taken" $ do
    events <- newIORef []
    renderToHtml (subscribing events) () >>= assertEqual "markup" "subscribed"
    recorded <- readIORef events
    assertWith ("the subscription ended: " <> show recorded) ("unsubscribed" `elem` recorded)
    assertWith ("the finalizer ran: " <> show recorded) ("finalized" `elem` recorded)

  it "runs a component in an application monad" $
    renderToHtmlWith (runMemDOM . flip runReaderT "configured") fromConfig ()
      >>= assertEqual "markup" "<p>configured</p>"

  it "fills a page's mount point, and only an unambiguous one" $ do
    assertEqual "filled"
      (Right "<body><div id=\"app\"><p>x</p></div><script></script></body>")
      (fillMountPoint "app" "<p>x</p>" "<body><div id=\"app\"></div><script></script></body>")
    assertWith "no mount point" (either (const True) (const False) (fillMountPoint "app" "x" "<body></body>"))
    assertWith "two mount points" (either (const True) (const False) (fillMountPoint "app" "x" "<div id=\"app\"></div><div id=\"app\"></div>"))

  it "embeds JSON in a script that nothing in it can end" $ do
    assertEqual "escaped"
      "<script type=\"application/json\" id=\"state\">{\"html\":\"\\u003c/script>\\u003c!-- x\"}</script>"
      (embed "state" "{\"html\":\"</script><!-- x\"}")
    assertEqual "the id escaped as an attribute"
      "<script type=\"application/json\" id=\"a&quot;b&amp;c\">1</script>"
      (embed "a\"b&c" "1")

  it "reads back what the page embedded, by its id" $ do
    -- Into the ambient document, which is the one readEmbedded searches.
    Just body <- runMemDOM (selectElement (QuerySelector "body"))
    H.HalogenSocket {dispose} <- runMemDOM (runUI (embedded "server-state" "{\"count\":3}") () body)
    found <- runMemDOM (readEmbedded "server-state")
    missing <- runMemDOM (readEmbedded "no-such-state")
    runMemDOM dispose
    assertEqual "the text" (Just "{\"count\":3}") found
    assertEqual "nothing" Nothing missing

#endif
