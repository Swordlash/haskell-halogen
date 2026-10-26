-- | The loopback in a real page: the whole pipeline, mirror, JSON, client,
-- the page's own events and the patches they cause, driven as a reader
-- would drive it.
module Test.Loopback (spec) where

import Control.Monad.UUID (MonadUUID)
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Remote.DOM (RemoteDOM)
import Halogen.Remote.Loopback (Loopback (..), Traffic (..))
import Halogen.Remote.Loopback qualified as Loopback
import Halogen.Subscription qualified as HS
import Protolude hiding (find)
import Test.Components
import Test.Hspec.Halogen
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

data HostQuery a = Dispose a

data Host = Host
  { label :: H.RefLabel
  , running :: Maybe Loopback
  }

-- | A page's component that shows a component of the wire's in its own
-- element, and notes the traffic.
host :: forall m q. (MonadIO m, MonadUUID m) => IORef [Traffic] -> H.Component q () Void RemoteDOM -> H.Component HostQuery () Void m
host traffic remote =
  H.component
    (\() -> (\label -> Host {label, running = Nothing}) <$> H.newRefLabel "remote")
    render
    [ H.onInitialize start
    , H.onFinalize stop
    , H.onQuery (\(Dispose next) -> stop $> Just next)
    ]
  where
    render :: Host -> H.ComponentHTML () Empty m
    render st = HH.div [HP.id "remote", HP.ref st.label] []
    start = do
      st <- get
      element <- H.getHTMLElementRef st.label
      for_ element $ \container -> do
        loopback <- liftIO (Loopback.mountIn container remote ())
        _ <- liftIO (HS.subscribe loopback.traffic (\t -> atomicModifyIORef' traffic (\ts -> (ts <> [t], ()))))
        modify (\s -> s {running = Just loopback})
    stop = do
      st <- get
      for_ st.running (liftIO . (.dispose))
      modify (\s -> s {running = Nothing})

-- | A page's component that tries to show a component of the wire's, and
-- says whether it could.
trying :: forall m q. (MonadIO m, MonadUUID m) => H.Component q () Void RemoteDOM -> H.Component H.VoidF () Void m
trying remote =
  H.component
    (\() -> (,"trying") <$> H.newRefLabel "remote")
    render
    [H.onInitialize start]
  where
    render :: (H.RefLabel, Text) -> H.ComponentHTML () Empty m
    render (label, outcome) = HH.div_ [HH.div [HP.id "remote", HP.ref label] [], HH.span [HP.id "outcome"] [HH.text outcome]]
    start = do
      (label, _) <- get
      element <- H.getHTMLElementRef label
      for_ element $ \container ->
        liftIO (try (Loopback.mountIn container remote ())) >>= \case
          Left (ErrorCall e) -> modify (second (const (toS e)))
          Right loopback -> liftIO loopback.dispose >> modify (second (const "mounted"))

spec :: forall m -> (MonadBrowserTest m, MonadUUID m) => Spec
spec m = do
  it "shows a component through the client, and patches it on the page's own clicks" $ runPage $ do
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic counter) ()
    find ui "#count" >>= (`shouldHaveText` "0")
    find ui "#increment" >>= click
    find ui "#count" >>= (`shouldHaveText` "1")
    find ui "#increment" >>= click
    find ui "#count" >>= (`shouldHaveText` "2")

  it "takes what is typed to the component" $ runPage $ do
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic echo) ()
    field <- find ui "#field"
    typeText field "hi"
    find ui "#echo" >>= (`shouldHaveText` "hi")
    getProperty field "value" >>= (`shouldBe` "hi")

  it "keeps the page from submitting a form it listens to" $ runPage $ do
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic form) ()
    find ui "#submit" >>= click
    find ui "#submitted" >>= (`shouldHaveText` "1")
    -- A navigation would have taken the page, and the test, with it.
    find ui "#submit" >>= click
    find ui "#submitted" >>= (`shouldHaveText` "2")

  it "keeps a child's element when its parent replaces the element around it" $ runPage $ do
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic swapping) ()
    poke <- find ui "#poke"
    click poke
    find ui "#poke" >>= (`shouldHaveText` "poked 1")
    find ui "#swap" >>= click
    eventually (find ui "#wrapper" >>= (`getProperty` "tagName") >>= (`shouldBe` "SECTION"))
    -- The child's button is the one that was there, moved into the
    -- section, and it still counts.
    moved <- find ui "#poke"
    moved `shouldBeSameElement` poke
    click moved
    find ui "#poke" >>= (`shouldHaveText` "poked 2")
    find ui "#swap" >>= click
    eventually (find ui "#wrapper" >>= (`getProperty` "tagName") >>= (`shouldBe` "DIV"))
    back <- find ui "#poke"
    back `shouldBeSameElement` poke
    click back
    find ui "#poke" >>= (`shouldHaveText` "poked 3")

  it "runs a handler that prevents the default and stops propagation" $ runPage $ do
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic guardedForm) ()
    find ui "#submit" >>= click
    find ui "#submitted" >>= (`shouldHaveText` "1")
    find ui "#submit" >>= click
    find ui "#submitted" >>= (`shouldHaveText` "2")

  it "lets its element go when the component fails to mount" $ runPage $ do
    ui <- mount m (trying failing) ()
    find ui "#outcome" >>= (`shouldHaveText` "no state to start from")
    container <- find ui "#remote"
    getAttribute container "data-halogen-remote" >>= (`shouldBe` Nothing)
    getProperty container "childElementCount" >>= (`shouldBe` "0")

  it "empties its element when disposed of, and showed the traffic both ways" $ runPage $ do
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic counter) ()
    find ui "#increment" >>= click
    find ui "#count" >>= (`shouldHaveText` "1")
    container <- find ui "#remote"
    getAttribute container "data-halogen-remote" >>= (`shouldBe` Just "attached")
    _ <- query ui (Dispose ())
    eventually (getProperty container "childElementCount" >>= (`shouldBe` "0"))
    getAttribute container "data-halogen-remote" >>= (`shouldBe` Nothing)
    seen <- unsafeIOToPageM (readIORef traffic)
    [() | ToPage _ <- seen] `shouldSatisfy` (not . null)
    [() | ToProgram _ <- seen] `shouldSatisfy` (not . null)
