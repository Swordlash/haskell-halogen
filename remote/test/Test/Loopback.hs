{-# LANGUAGE TemplateHaskell #-}

-- | The loopback in a real page: the whole pipeline, mirror, JSON, client,
-- the page's own events and the patches they cause, driven as a reader
-- would drive it.
module Test.Loopback (spec) where

import Control.Monad.UUID (MonadUUID)
import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.Activity qualified as Activity
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText, toJSText)
import Halogen.JSBits.Promise (Promise, wait)
import Halogen.Remote.DOM (RemoteDOM)
import Halogen.Remote.Loopback (Loopback (..), Traffic (..))
import Halogen.Remote.Loopback qualified as Loopback
import Halogen.Subscription qualified as HS
import Protolude hiding (find, wait)
import Test.Components
import Test.Hspec.Halogen
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

$( browserJS
     ["test/jsbits/probe.js"]
     [ ("js_probe_set", "remote_probe_set", Safe, [t|IO ()|])
     , ("js_probe_ask", "remote_probe_ask", Safe, [t|IO (Promise JSText)|])
     , ("js_probe_askers", "remote_probe_askers", Safe, [t|IO Int|])
     , ("js_active_id", "remote_active_id", Safe, [t|IO JSText|])
     , ("js_storage_get", "remote_storage_get", Safe, [t|JSText -> IO JSText|])
     , ("js_storage_remove", "remote_storage_remove", Safe, [t|JSText -> IO ()|])
     , ("js_address", "remote_address", Safe, [t|IO JSText|])
     , ("js_drop_fragment", "remote_drop_fragment", Safe, [t|IO ()|])
     , ("js_back", "remote_back", Safe, [t|IO (Promise JSText)|])
     ]
 )

-- | Trees, running, waiting, as the page's probe answers.
probe :: PageM s (Int, Int, Int)
probe = unsafeIOToPageM $ do
  answer <- fromJSText <$> (wait =<< js_probe_ask)
  case mapM (readMaybe . T.unpack) (T.words answer) of
    Just [t, r, w] -> pure (t, r, w)
    _ -> panic ("the probe answered " <> answer)

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

  it "keeps the page from following a link's plain click, which the component follows" $ runPage $ do
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic linking) ()
    find ui "#link" >>= click
    find ui "#followed" >>= (`shouldHaveText` "1")
    -- Following the link would have taken the page, and the test, with it.
    find ui "#link" >>= click
    find ui "#followed" >>= (`shouldHaveText` "2")

  it "keeps from being typed only the key a handler declared, which the component hears" $ runPage $ do
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic searching) ()
    field <- find ui "#query"
    typeText field "a/b"
    find ui "#searched" >>= (`shouldHaveText` "1")
    getProperty field "value" >>= (`shouldBe` "ab")

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

  it "adds nothing to the page's probe: the program in the page answers for the loopback's tree, once" $ runPage $ do
    unsafeIOToPageM js_probe_set
    traffic <- unsafeIOToPageM (newIORef [])
    -- The host's tree and the loopback's, both this program's: core gives
    -- the probe its one asker as the first is mounted, and the loopback's
    -- client gives it none.
    ui <- mount m (host traffic counter) ()
    find ui "#count" >>= (`shouldHaveText` "0")
    unsafeIOToPageM js_probe_askers `shouldReturn` 1
    (trees, _, _) <- probe
    inProgram <- unsafeIOToPageM Activity.activity
    trees `shouldBe` inProgram.trees
    _ <- query ui (Dispose ())
    -- The loopback's tree is gone, and counted gone once.
    (fewer, _, _) <- probe
    fewer `shouldBe` trees - 1

  -- In the loopback the program is in the page, but its handles are the
  -- session's node numbers, not the page's elements, and in a browser a
  -- handle is the engine's value, which answers for itself only natively:
  -- a checked cast of one ('H.getHTMLElementRef', a dialog's fromElement)
  -- is no cast, so what a component does to the page through the Web types
  -- is not carried out, neither here nor through the wire, rather than
  -- twice. The component goes on.
  it "carries out nothing a component does to the page through the Web types, and goes on" $ runPage $ do
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic operating) ()
    _ <- find ui "#first"
    (fromJSText <$> unsafeIOToPageM js_active_id) >>= (`shouldSatisfy` (/= "first"))
    find ui "#focus-second" >>= click
    (fromJSText <$> unsafeIOToPageM js_active_id) >>= (`shouldSatisfy` (/= "second"))
    find ui "#open" >>= click
    find ui "#dialog" >>= (`getProperty` "open") >>= (`shouldBe` "false")
    find ui "#forget" >>= click
    eventually (find ui "#far" >>= (`getProperty` "nextElementSibling") >>= (`shouldBe` "null"))

  it "lets its element go when the component fails to mount" $ runPage $ do
    ui <- mount m (trying failing) ()
    find ui "#outcome" >>= (`shouldHaveText` "no state to start from")
    container <- find ui "#remote"
    getAttribute container "data-halogen-remote" >>= (`shouldBe` Nothing)
    getProperty container "childElementCount" >>= (`shouldBe` "0")

  -- The program is in the page: what the component stores is the page's
  -- own storage, through the session's mirror (which the client's first
  -- message fills) and the client's writes, and a router reads and writes
  -- the page's own address.
  it "keeps what a component stores in the page's own localStorage, which the next mount reads" $ runPage $ do
    unsafeIOToPageM (js_storage_remove (toJSText "haskell-halogen:count"))
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic storing) ()
    find ui "#count" >>= (`shouldHaveText` "0")
    find ui "#bump" >>= click
    find ui "#count" >>= (`shouldHaveText` "1")
    -- 1, as JSON, in base64, written by the client.
    eventually (fromJSText <$> unsafeIOToPageM (js_storage_get (toJSText "haskell-halogen:count")) >>= (`shouldBe` "MQ=="))
    _ <- query ui (Dispose ())
    again <- mount m (host traffic storing) ()
    -- Read through the session's mirror, which the client's first message
    -- filled before the component was mounted.
    find again "#count" >>= (`shouldHaveText` "1")
    unsafeIOToPageM (js_storage_remove (toJSText "haskell-halogen:count"))

  it "routes the page's own address with a hash router, back included" $ runPage $ do
    unsafeIOToPageM js_drop_fragment
    traffic <- unsafeIOToPageM (newIORef [])
    ui <- mount m (host traffic hashRouting) ()
    find ui "#spot" >>= (`shouldHaveText` "Front")
    find ui "#to-post" >>= click
    find ui "#spot" >>= (`shouldHaveText` "Post 1")
    (fromJSText <$> unsafeIOToPageM js_address) >>= (`shouldSatisfy` ("#/blog/post/1" `T.isSuffixOf`))
    find ui "#to-about" >>= click
    find ui "#spot" >>= (`shouldHaveText` "About")
    _ <- unsafeIOToPageM (wait =<< js_back)
    eventually (find ui "#spot" >>= (`shouldHaveText` "Post 1"))
    _ <- query ui (Dispose ())
    unsafeIOToPageM js_drop_fragment

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
