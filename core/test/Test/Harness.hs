{-# LANGUAGE CPP #-}

-- | "Halogen.Test.Native": a test mounts a component in a page of its own,
-- acts on it as the reader does, waits for the program, and reads the tree.
-- Every view is disposed of by its test, so this spec runs anywhere in the
-- suite.
module Test.Harness (spec) where

import Prelude

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))

import Test.Hspec (Spec, xdescribe)

spec :: Spec
spec = xdescribe "the harness for the in-memory DOM" $ pure ()

#else

import Control.Concurrent (forkIO, rtsSupportsBoundThreads, setNumCapabilities, threadDelay)
import Control.Concurrent.MVar (modifyMVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Class (get, modify, put)
import DOM.HTML.Indexed qualified as I
import Data.Foreign (foreignToBool)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Query.Event qualified as HQE
import Halogen.Subscription qualified as HS
import Halogen.Test.Native qualified as T
import Halogen.VDom.DOM.HandlerOptions (KeyMatch (..), keyMatch)
import Halogen.VDom.DOM.Monad qualified as DOM
import Halogen.VDom.DOM.Monad.Native qualified as N
import System.Mem (performMajorGC)
import System.Mem.Weak (deRefWeak)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Web.DOM.Element qualified as Element
import Web.Event.Event (EventType (..))
import Web.Event.Event qualified as Event
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent qualified as KE

-- | Note a line in a log, at once.
note :: (MonadIO m) => IORef [Text] -> Text -> H.HalogenM s a ps o m ()
note ref line = H.sync (liftIO (atomicModifyIORef' ref (\before -> (before <> [line], ()))))

-- | A guard against a hang, with the view's HTML in the message.
guarded :: T.View q i o -> IO a -> IO a
guarded view = T.within (("the view:\n" <>) <$> T.html view)

-- | A 'T.Failure''s message, or a failure of the test if none was thrown.
failure :: IO a -> IO String
failure io =
  try io >>= \case
    Left (T.Failure message) -> pure (Text.unpack message)
    Right _ -> fail "no Failure was thrown"

--------------------------------------------------------------------------------
-- A counter: queries, input, outputs and a finalizer
--------------------------------------------------------------------------------

data Counter = Counter {label :: Text, count :: Int}

data CounterQuery a = Count (Int -> a) | Bump a

data CounterAction = Increment

-- | A counter with a label from its input. A click raises the new count.
-- It notes each input it receives, and its end.
counter :: forall m. (MonadIO m) => IORef [Text] -> H.Component CounterQuery Text Int m
counter logged =
  H.component
    (\label -> pure Counter {label, count = 0})
    render
    [ H.onAction $ \Increment -> do
        modify (\s -> s {count = s.count + 1})
        s <- get
        H.raise s.count
    , H.onQuery $ \case
        Count reply -> Just . reply . (.count) <$> get
        Bump a -> Just a <$ modify (\s -> s {count = s.count + 1})
    , H.onReceive $ \label -> do
        note logged ("receive " <> label)
        modify (\s -> s {label})
    , H.onFinalize (note logged "finalize")
    ]
  where
    render :: Counter -> H.ComponentHTML CounterAction Empty m
    render s =
      HH.div_
        [ HH.span [HP.class_ (HH.ClassName "label")] [HH.text s.label]
        , HH.span [HP.class_ (HH.ClassName "count")] [HH.text (Text.pack (show s.count))]
        , HH.button [HP.class_ (HH.ClassName "inc"), HE.onClick (const Increment)] [HH.text "+"]
        ]

-- | Use a view and drop it. The result says whether the view's page has
-- been collected: the page holds its document.
usedAndDropped :: IO (IO Bool)
usedAndDropped = do
  logged <- newIORef []
  weak <- T.withView id (counter logged) "a" $ \view -> do
    T.click view ".inc"
    mkWeakIORef (N.pageDocument view.page).kids (pure ())
  pure (isNothing <$> deRefWeak weak)
{-# NOINLINE usedAndDropped #-}

--------------------------------------------------------------------------------
-- What a click is made of
--------------------------------------------------------------------------------

-- | A button that notes every event of a click, and a popup that closes
-- on a press outside it. The popup holds a button of its own.
data Popup = Popup {shown :: Bool}

data PopupAction = Watch | Heard Text | Close

popup :: forall m. (DOM.MonadBrowserDOM m, MonadUnliftIO m) => IORef [Text] -> H.Component H.VoidF () Void m
popup logged =
  H.component
    (\() -> pure Popup {shown = True})
    render
    [ H.onInitialize (act Watch)
    , H.onAction act
    ]
  where
    events = ["pointerdown", "mousedown", "pointerup", "mouseup", "click"]
    noting = [HE.handler (EventType e) (const (Heard e)) | e <- events]
    render :: Popup -> H.ComponentHTML PopupAction Empty m
    render s =
      HH.div_
        [ HH.button (HP.class_ (HH.ClassName "plain") : noting) [HH.text "plain"]
        , if s.shown
            then HH.div [HP.class_ (HH.ClassName "popup")] [HH.button [HP.class_ (HH.ClassName "choose"), HE.onClick (const (Heard "chosen"))] [HH.text "choose"]]
            else HH.text ""
        ]
    act = \case
      Watch -> do
        presses <- H.sync $ do
          doc <- DOM.window >>= DOM.document
          HS.lowerEmitter (HQE.eventListener (EventType "pointerdown") (N.fromNative (N.toNative doc)) outside)
        void (H.subscribe presses)
      Heard e -> note logged e
      Close -> do
        note logged "closed"
        put Popup {shown = False}
    -- A press whose target is not in the popup closes it.
    outside ev = case Event.target ev >>= Element.fromEventTarget of
      Just target | isNothing (Element.closestNow target ".popup") -> Just Close
      _ -> Nothing

--------------------------------------------------------------------------------
-- Checkboxes, radio buttons and forms
--------------------------------------------------------------------------------

data FormAction = Noted Text | Once

-- | A form with checkboxes, radio buttons and buttons, each noting what it
-- hears. The click listeners note the @checked@ they read.
form :: forall m. (MonadIO m) => IORef [Text] -> H.Component H.VoidF () Void m
form logged =
  H.component
    (\() -> pure False)
    render
    [ H.onAction $ \case
        Noted line -> note logged line
        Once -> put True
    ]
  where
    render :: Bool -> H.ComponentHTML FormAction Empty m
    render used =
      HH.form
        [HE.onSubmit (const (Noted "submit"))]
        [ checkbox "free" False False
        , checkbox "stuck" False True
        , mixed "mixed" False
        , mixed "mixedstuck" True
        , radio "small" False False
        , radio "medium" True False
        , radio "large" False False
        , radio "fixed" False True
        , HH.button [HP.class_ (HH.ClassName "go")] [HH.text "go"]
        , HH.button [HP.class_ (HH.ClassName "plain"), HP.type_ I.ButtonButton] [HH.text "plain"]
        , HH.button [HP.class_ (HH.ClassName "stop"), HE.preventingDefault (HE.onClick (const (Noted "stop")))] [HH.text "stop"]
        , HH.input [HP.class_ (HH.ClassName "send"), HP.type_ I.InputSubmit]
        , HH.button
            [ HP.class_ (HH.ClassName "held")
            , HP.type_ I.ButtonButton
            , HE.preventingDefault (HE.handler (EventType "pointerdown") (const (Noted "held pointerdown")))
            , HE.handler (EventType "mousedown") (const (Noted "held mousedown"))
            , HE.handler (EventType "mouseup") (const (Noted "held mouseup"))
            , HE.onClick (const (Noted "held click"))
            ]
            [HH.text "held"]
        , HH.button [HP.class_ (HH.ClassName "wrap")] [HH.span [HP.class_ (HH.ClassName "icon")] [HH.text "go"]]
        , HH.button [HP.class_ (HH.ClassName "once"), HP.disabled used, HE.onClick (const Once)] [HH.text "once"]
        , HH.button
            [ HP.class_ (HH.ClassName "off")
            , HP.disabled True
            , HE.handler (EventType "pointerdown") (const (Noted "off pointerdown"))
            , HE.handler (EventType "mousedown") (const (Noted "off mousedown"))
            , HE.handler (EventType "pointerup") (const (Noted "off pointerup"))
            , HE.handler (EventType "mouseup") (const (Noted "off mouseup"))
            , HE.onClick (const (Noted "off click"))
            ]
            [HH.text "off"]
        ]
    checkbox name on prevent =
      HH.input
        [ HP.class_ (HH.ClassName name)
        , HP.type_ I.InputCheckbox
        , HP.checked on
        , clicked prevent name
        , HE.handler (EventType "input") (const (Noted (name <> " input")))
        , HE.onChecked (\b -> Noted (name <> " change " <> shown b))
        ]
    -- A checkbox that starts indeterminate. Its click listener notes the
    -- indeterminate it reads.
    mixed name prevent =
      HH.input
        [ HP.class_ (HH.ClassName name)
        , HP.type_ I.InputCheckbox
        , HP.prop (HH.PropName "indeterminate") True
        , (if prevent then HE.preventingDefault else id)
            (HE.addForeignPropHandler (EventType "click") "indeterminate" (Just . foreignToBool) (\b -> Noted (name <> " click " <> shown b)))
        ]
    radio name on prevent =
      HH.input
        [ HP.class_ (HH.ClassName name)
        , HP.type_ I.InputRadio
        , HP.name "size"
        , HP.checked on
        , clicked prevent name
        , HE.handler (EventType "input") (const (Noted (name <> " input")))
        , HE.onChecked (\b -> Noted (name <> " change " <> shown b))
        ]
    -- A click listener that notes the checked it reads, and may prevent
    -- the default.
    clicked prevent name =
      (if prevent then HE.preventingDefault else id)
        (HE.addForeignPropHandler (EventType "click") "checked" (Just . foreignToBool) (\b -> Noted (name <> " click " <> shown b)))
    shown b = if b then "on" else "off"

-- | A form with a required text field and a required checkbox, a submit
-- button, and one that skips the check.
signup :: forall m. (MonadIO m) => IORef [Text] -> H.Component H.VoidF () Void m
signup logged =
  H.component
    (\() -> pure ())
    render
    [ H.onAction $ \case
        Noted line -> note logged line
        Once -> pure ()
    ]
  where
    render :: () -> H.ComponentHTML FormAction Empty m
    render () =
      HH.form
        [HE.onSubmit (const (Noted "submit"))]
        [ HH.input
            [ HP.class_ (HH.ClassName "name")
            , HP.required True
            , HE.handler (EventType "invalid") (const (Noted "invalid name"))
            ]
        , HH.input
            [ HP.class_ (HH.ClassName "agree")
            , HP.type_ I.InputCheckbox
            , HP.required True
            , HE.handler (EventType "invalid") (const (Noted "invalid agree"))
            ]
        , HH.button [HP.class_ (HH.ClassName "join")] [HH.text "join"]
        , HH.button [HP.class_ (HH.ClassName "skip"), HP.attr (HH.AttrName "formnovalidate") ""] [HH.text "skip"]
        ]

--------------------------------------------------------------------------------
-- A field, and a program that waits
--------------------------------------------------------------------------------

data FieldQuery a = Typed (Text -> a)

data FieldAction = Type Text | Save

data Field = Field {typed :: Text, saved :: Bool}

-- | A field whose text the component keeps, and a button that saves after
-- a wait in @H.async@.
field :: forall m. (MonadIO m) => H.Component FieldQuery () Void m
field =
  H.component
    (\() -> pure Field {typed = "", saved = False})
    render
    [ H.onAction $ \case
        Type t -> modify (\s -> s {typed = t})
        Save -> do
          H.async (liftIO (threadDelay 20_000))
          modify (\s -> s {saved = True})
    , H.onQuery $ \case
        Typed reply -> Just . reply . (.typed) <$> get
    ]
  where
    render :: Field -> H.ComponentHTML FieldAction Empty m
    render s =
      HH.div_
        [ HH.input [HP.class_ (HH.ClassName "name"), HE.onValueInput Type]
        , HH.input [HP.class_ (HH.ClassName "fixed"), HP.readOnly True, HE.onValueInput Type]
        , HH.input [HP.class_ (HH.ClassName "box"), HP.type_ I.InputCheckbox]
        , HH.button [HP.class_ (HH.ClassName "save"), HE.onClick (const Save)] [HH.text "save"]
        , HH.p [HP.class_ (HH.ClassName "status")] [HH.text (if s.saved then "saved" else "editing")]
        ]

--------------------------------------------------------------------------------
-- Asks a gate holds
--------------------------------------------------------------------------------

data Asking = Asking {answers :: [Text], clicks :: Int}

data AskAction = Ask Text | Again

-- | Asks twice as it starts, each on a fork of its own, and once more on
-- every click. Each ask is held on the gate until the test answers it.
asking :: forall m. (MonadIO m) => T.Gate Text Text -> H.Component H.VoidF () Void m
asking gate =
  H.component
    (\() -> pure Asking {answers = [], clicks = 0})
    render
    [ H.onInitialize $ do
        void (H.fork (act (Ask "first")))
        void (H.fork (act (Ask "second")))
    , H.onAction act
    ]
  where
    act = \case
      Ask what -> do
        r <- H.async (liftIO (T.hold gate what))
        modify (\s -> s {answers = s.answers <> [r]})
      Again -> do
        _ <- H.async (liftIO (T.hold gate "again"))
        modify (\s -> s {clicks = s.clicks + 1})
    render :: Asking -> H.ComponentHTML AskAction Empty m
    render s =
      HH.div_
        [ HH.p [HP.class_ (HH.ClassName "answers")] [HH.text (Text.intercalate "," s.answers)]
        , HH.p [HP.class_ (HH.ClassName "clicks")] [HH.text (Text.pack (show s.clicks))]
        , HH.button [HP.class_ (HH.ClassName "again"), HE.onClick (const Again)] [HH.text "again"]
        ]

--------------------------------------------------------------------------------
-- Held waits and the test's clock
--------------------------------------------------------------------------------

data Loading = Loading {loaded :: Bool, ticked :: Bool}

data LoadingAction = Tick

-- | Loads in an @H.async@ as it starts, and ticks after a counted pause of
-- five seconds when clicked.
loading :: forall m. (MonadIO m) => H.Component H.VoidF () Void m
loading =
  H.component
    (\() -> pure Loading {loaded = False, ticked = False})
    render
    [ H.onInitialize $ do
        H.async (liftIO (threadDelay 1000))
        modify (\s -> s {loaded = True})
    , H.onAction $ \Tick -> do
        H.delayCounted 5
        modify (\s -> s {ticked = True})
    ]
  where
    render :: Loading -> H.ComponentHTML LoadingAction Empty m
    render s =
      HH.div_
        [ HH.p [HP.class_ (HH.ClassName "loaded")] [HH.text (if s.loaded then "yes" else "no")]
        , HH.p [HP.class_ (HH.ClassName "ticked")] [HH.text (if s.ticked then "yes" else "no")]
        , HH.button [HP.class_ (HH.ClassName "tick"), HE.onClick (const Tick)] [HH.text "tick"]
        ]

--------------------------------------------------------------------------------
-- Keys
--------------------------------------------------------------------------------

data KeyAction = Down KeyboardEvent | Up KeyboardEvent | CtrlS

-- | A field that notes each key event with the keys held. Around it, a
-- handler that hears only Ctrl+S.
typing :: forall m. (MonadIO m) => IORef [Text] -> H.Component H.VoidF () Void m
typing logged =
  H.component
    (\() -> pure ())
    render
    [ H.onAction $ \case
        Down e -> note logged (shown "keydown" e)
        Up e -> note logged (shown "keyup" e)
        CtrlS -> note logged "save"
    ]
  where
    shown name e =
      Text.unwords
        (name : KE.key e : [held | (held, True) <- [("alt", KE.altKey e), ("ctrl", KE.ctrlKey e), ("meta", KE.metaKey e), ("shift", KE.shiftKey e)]])
    render :: () -> H.ComponentHTML KeyAction Empty m
    render () =
      HH.div
        [HE.matching (HE.keysWith [(keyMatch "s") {ctrlKey = Just True}]) (HE.onKeyDown (const CtrlS))]
        [HH.input [HP.class_ (HH.ClassName "box"), HE.onKeyDown Down, HE.onKeyUp Up]]

--------------------------------------------------------------------------------
-- Views side by side
--------------------------------------------------------------------------------

data Bell = Bell {open :: Bool, heard :: Int}

data BellAction = Listen | Ring | Pressed

data BellQuery a = Report ((Bool, Int) -> a)

-- | A bell. A click on it opens its list. A @pointerdown@ anywhere in the
-- document closes it and is counted.
bell :: forall m. (DOM.MonadBrowserDOM m, MonadUnliftIO m) => H.Component BellQuery () Void m
bell =
  H.component
    (\() -> pure Bell {open = False, heard = 0})
    render
    [ H.onInitialize (act Listen)
    , H.onAction act
    , H.onQuery $ \case
        Report reply -> do
          s <- get
          pure (Just (reply (s.open, s.heard)))
    ]
  where
    render :: Bell -> H.ComponentHTML BellAction Empty m
    render _ = HH.button [HP.class_ (HH.ClassName "bell"), HE.onClick (const Ring)] [HH.text "bell"]
    act = \case
      Listen -> do
        presses <- H.sync $ do
          doc <- DOM.window >>= DOM.document
          HS.lowerEmitter (HQE.eventListener (EventType "pointerdown") (N.fromNative (N.toNative doc)) Just)
        void (H.subscribe (Pressed <$ presses))
      Ring -> modify (\s -> Bell {open = True, heard = s.heard})
      Pressed -> modify (\s -> Bell {open = False, heard = s.heard + 1})

-- | Run each action on its own thread, all at once, and wait for them all.
inParallel :: [IO a] -> IO [a]
inParallel actions = do
  results <- for actions $ \action -> do
    result <- newEmptyMVar
    void (forkIO (try action >>= putMVar result))
    pure result
  for results $ \result -> takeMVar result >>= either (throwIO @SomeException) pure

-- | A barrier for this many threads. Each thread waits until all arrive.
barrier :: Int -> IO (IO ())
barrier n = do
  arrived <- newMVar (0 :: Int)
  everyone <- newEmptyMVar
  pure $ do
    c <- modifyMVar arrived (\c -> pure (c + 1, c + 1))
    when (c == n) (putMVar everyone ())
    readMVar everyone

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "the harness for the in-memory DOM" $ do
  describe "a view" $ do
    it "mounts a component whose monad is a transformer over MemDOM" $ do
      logged <- newIORef []
      T.withView (`runReaderT` ()) (counter logged) "a" $ \view -> do
        T.click view "button.inc"
        T.request view Count `shouldReturn` 1
        T.outputs view `shouldReturn` [1]

    it "is mounted, read, asked, and disposed of" $ do
      logged <- newIORef []
      page <- N.newPage
      view <- T.mountIn page id (counter logged) "a"
      T.textOf view ".label" `shouldReturn` "a"
      T.click view ".inc"
      T.click view ".inc"
      T.request view Count `shouldReturn` 2
      T.tell view Bump
      T.request view Count `shouldReturn` 3
      T.textOf view ".count" `shouldReturn` "3"
      T.outputs view `shouldReturn` [1, 2]
      T.html view `shouldReturn` "<div><span class=\"label\">a</span><span class=\"count\">3</span><button class=\"inc\">+</button></div>"
      T.dispose view
      readIORef logged `shouldReturn` ["finalize"]
      N.isConnected view.container `shouldReturn` False
      N.querySelector ".inc" (N.pageDocument page) >>= (`shouldBe` True) . isNothing
      T.query view (Count id) >>= (`shouldBe` True) . isNothing
      -- Disposing twice does nothing more.
      T.dispose view
      readIORef logged `shouldReturn` ["finalize"]

    it "drops its page" $ do
      gone <- usedAndDropped
      performMajorGC
      performMajorGC
      gone `shouldReturn` True

    it "gives the component a new input, the same one twice too" $ do
      logged <- newIORef []
      T.withView id (counter logged) "a" $ \view -> do
        T.setInput view "b"
        T.textOf view ".label" `shouldReturn` "b"
        T.setInput view "b"
        readIORef logged `shouldReturn` ["receive b", "receive b"]

  describe "a selector that finds nothing" $ do
    it "throws, naming the selector and showing the view" $ do
      logged <- newIORef []
      T.withView id (counter logged) "a" $ \view -> do
        clicked <- failure (T.click view "button.missing")
        ("click" `isInfixOf` clicked, "button.missing" `isInfixOf` clicked, "<button class=\"inc\">" `isInfixOf` clicked)
          `shouldBe` (True, True, True)
        read' <- failure (T.textOf view ".missing")
        ("textOf" `isInfixOf` read', ".missing" `isInfixOf` read') `shouldBe` (True, True)
        typed <- failure (T.typeInto view "input.missing" "x")
        ("typeInto" `isInfixOf` typed, "input.missing" `isInfixOf` typed) `shouldBe` (True, True)
        T.find view ".missing" >>= (`shouldBe` True) . isNothing

  describe "a click" $ do
    it "is pointerdown, mousedown, pointerup, mouseup and click, in order" $ do
      logged <- newIORef []
      T.withView id (popup logged) () $ \view -> do
        T.click view "button.plain"
        -- The press is outside the popup: the document hears it after the
        -- button, and closes the popup.
        readIORef logged `shouldReturn` ["pointerdown", "closed", "mousedown", "pointerup", "mouseup", "click"]

    it "reaches a button in a popup that closes on a press outside it" $ do
      logged <- newIORef []
      T.withView id (popup logged) () $ \view -> do
        T.click view ".popup button.choose"
        readIORef logged `shouldReturn` ["chosen"]
        T.clickOutside view
        readIORef logged `shouldReturn` ["chosen", "closed"]
        T.find view ".popup" >>= (`shouldBe` True) . isNothing

    it "has no mousedown or mouseup when a listener prevents the pointerdown" $ do
      logged <- newIORef []
      T.withView id (form logged) () $ \view -> do
        T.click view "button.held"
        readIORef logged `shouldReturn` ["held pointerdown", "held click"]

  describe "a checkbox" $ do
    it "is ticked before the click listeners, then gets input and change" $ do
      logged <- newIORef []
      T.withView id (form logged) () $ \view -> do
        T.click view ".free"
        readIORef logged `shouldReturn` ["free click on", "free input", "free change on"]
        (T.checked =<< T.element view ".free") `shouldReturn` True
        T.click view ".free"
        (T.checked =<< T.element view ".free") `shouldReturn` False

    it "is put back when a click listener prevents the default" $ do
      logged <- newIORef []
      T.withView id (form logged) () $ \view -> do
        T.click view ".stuck"
        readIORef logged `shouldReturn` ["stuck click on"]
        (T.checked =<< T.element view ".stuck") `shouldReturn` False

    it "stops being indeterminate, unless a click listener prevents the default" $ do
      logged <- newIORef []
      T.withView id (form logged) () $ \view -> do
        T.click view ".mixed"
        readIORef logged `shouldReturn` ["mixed click off"]
        (T.property "indeterminate" =<< T.element view ".mixed") `shouldReturn` Just (T.ScalarBool False)
        T.click view ".mixedstuck"
        (T.property "indeterminate" =<< T.element view ".mixedstuck") `shouldReturn` Just (T.ScalarBool True)
        (T.checked =<< T.element view ".mixedstuck") `shouldReturn` False

  describe "a radio button" $ do
    it "is checked, unchecks the others of its group, and gets input and change" $ do
      logged <- newIORef []
      T.withView id (form logged) () $ \view -> do
        T.click view ".large"
        readIORef logged `shouldReturn` ["large click on", "large input", "large change on"]
        for ["small", "medium", "large", "fixed"] (\r -> T.checked =<< T.element view ("." <> r))
          `shouldReturn` [False, False, True, False]
        -- Checked already: no input or change.
        T.click view ".large"
        readIORef logged `shouldReturn` ["large click on", "large input", "large change on", "large click on"]

    it "puts its group back when a click listener prevents the default" $ do
      logged <- newIORef []
      T.withView id (form logged) () $ \view -> do
        T.click view ".fixed"
        readIORef logged `shouldReturn` ["fixed click on"]
        for ["small", "medium", "large", "fixed"] (\r -> T.checked =<< T.element view ("." <> r))
          `shouldReturn` [False, True, False, False]

  describe "a submit button" $ do
    it "submits its form, unless its click is prevented or it is not a submit button" $ do
      logged <- newIORef []
      T.withView id (form logged) () $ \view -> do
        T.click view "button.go"
        readIORef logged `shouldReturn` ["submit"]
        T.click view "input.send"
        readIORef logged `shouldReturn` ["submit", "submit"]
        T.click view "button.plain"
        T.click view "button.stop"
        readIORef logged `shouldReturn` ["submit", "submit", "stop"]

    it "submits its form when the click is on a node inside it" $ do
      logged <- newIORef []
      T.withView id (form logged) () $ \view -> do
        T.click view "button.wrap .icon"
        readIORef logged `shouldReturn` ["submit"]

    it "submits nothing when a click listener disables it, as in a browser" $ do
      logged <- newIORef []
      T.withView id (form logged) () $ \view -> do
        T.click view "button.once"
        (N.disabledControl =<< T.element view "button.once") `shouldReturn` True
        readIORef logged `shouldReturn` []

    it "submits nothing while a required field is empty, and tells the field" $ do
      logged <- newIORef []
      T.withView id (signup logged) () $ \view -> do
        T.click view "button.join"
        readIORef logged `shouldReturn` ["invalid name", "invalid agree"]
        T.typeInto view "input.name" "Ada"
        T.click view "button.join"
        readIORef logged `shouldReturn` ["invalid name", "invalid agree", "invalid agree"]
        T.click view "input.agree"
        T.click view "button.join"
        readIORef logged `shouldReturn` ["invalid name", "invalid agree", "invalid agree", "submit"]

    it "submits with a required field empty when the button says formnovalidate" $ do
      logged <- newIORef []
      T.withView id (signup logged) () $ \view -> do
        T.click view "button.skip"
        readIORef logged `shouldReturn` ["submit"]

    it "gets only the pointer events when it is disabled, and submits nothing" $ do
      logged <- newIORef []
      T.withView id (form logged) () $ \view -> do
        T.click view "button.off"
        readIORef logged `shouldReturn` ["off pointerdown", "off pointerup"]

  describe "typing" $ do
    it "sets the field's value and sends input" $ do
      T.withView id field () $ \view -> do
        T.typeInto view "input.name" "Ada"
        T.request view Typed `shouldReturn` "Ada"
        (T.property "value" =<< T.element view "input.name") >>= (`shouldBe` "Just (ScalarText \"Ada\")") . show
        T.typeInto view "input.name" ""
        T.request view Typed `shouldReturn` ""

    it "leaves a read-only field as it is, and refuses what is not a text field" $ do
      T.withView id field () $ \view -> do
        T.typeInto view "input.fixed" "no"
        T.request view Typed `shouldReturn` ""
        refused <- failure (T.typeInto view "input.box" "x")
        ("not a text field" `isInfixOf` refused) `shouldBe` True

  describe "a key press" $ do
    it "says which keys are held, in keydown and in keyup, to a handler that asks for them too" $ do
      logged <- newIORef []
      T.withView id (typing logged) () $ \view -> do
        T.press view "input.box" "s"
        T.pressWith [T.Ctrl] view "input.box" "s"
        T.pressWith [T.Shift, T.Alt, T.Meta] view "input.box" "Enter"
        readIORef logged
          `shouldReturn` [ "keydown s"
                         , "keyup s"
                         , "keydown s ctrl"
                         , "save"
                         , "keyup s ctrl"
                         , "keydown Enter alt meta shift"
                         , "keyup Enter alt meta shift"
                         ]

  describe "settle" $ do
    it "waits for what a handler waits for in H.async" $ do
      T.withView id field () $ \view -> do
        T.click view "button.save"
        guarded view (T.settle view)
        T.textOf view ".status" `shouldReturn` "saved"

    it "with a deadline lets a quiet view pass, and says what a view that waits is doing" $ do
      gate <- T.newGate
      T.withView id (asking gate) () $ \view -> do
        T.settleHoldingWithin 5 view (T.holds gate)
        late <- failure (T.settleWithin 0.2 view)
        ("gave up after 0.2 seconds: the view is not quiet: 0 running or queued, 2 waiting. The view's HTML:" `isInfixOf` late) `shouldBe` True
        ("answers" `isInfixOf` late) `shouldBe` True
        -- No gate is named, so nothing accounts for the two waits.
        unheld <- failure (T.settleHoldingWithin 0.2 view mempty)
        ("the view has not settled: 0 running or queued, 2 waiting, and the gates hold 0." `isInfixOf` unheld) `shouldBe` True
        T.held gate >>= mapM_ (\h -> T.answer gate h h.asked)
        T.settleWithin 5 view
        T.textOf view ".answers" >>= (`shouldBe` 2) . length . Text.splitOn ","

    it "gives up on a gate that never holds what the test waits for" $ do
      gate <- T.newGate @Text @Text
      waited <- failure (T.waitHeldWithin 0.1 gate (not . null))
      waited `shouldBe` "gave up after 0.1 seconds: the gate does not hold the asks the test waits for. It holds 0."

    it "gives up on any other wait after the time the test names" $ do
      never <- newEmptyMVar
      waited <- failure (T.withinFor 0.1 (pure "nothing came") (takeMVar never :: IO ()))
      waited `shouldBe` "gave up after 0.1 seconds: nothing came"

  describe "a view's activity" $ do
    it "counts the view's own tree, and not a view beside it" $ do
      gate <- T.newGate
      T.withView id (asking gate) () $ \busy ->
        T.withView id field () $ \calm -> do
          guarded busy (T.waitActivity busy (\a -> a.running == 0 && a.waiting == 2))
          T.activity busy `shouldReturn` T.Activity {trees = 1, running = 0, waiting = 2}
          guarded calm (T.settle calm)
          T.activity calm `shouldReturn` T.Activity {trees = 1, running = 0, waiting = 0}
          -- The view beside it waits, and this one does not wait for it.
          late <- failure (T.waitActivityWithin 0.1 calm (\a -> a.waiting == 2))
          ("the view is not doing what the test waits for: 0 running or queued, 0 waiting." `isInfixOf` late) `shouldBe` True

    it "reads no tree once the view is disposed of" $ do
      logged <- newIORef []
      view <- T.withView id (counter logged) "a" pure
      T.activity view `shouldReturn` T.Activity {trees = 0, running = 0, waiting = 0}

  describe "a gate" $ do
    it "holds asks until the test answers them, in the order it chooses" $ do
      gate <- T.newGate
      T.withView id (asking gate) () $ \view -> do
        guarded view (T.settleHolding view (T.holds gate))
        asks <- T.held gate
        map (.number) asks `shouldBe` [1, 2]
        -- The two asks come from two forks, so either can reach the gate
        -- first. Tell them apart by what they ask.
        let named what = filter (\h -> h.asked == what) asks
        case named "first" <> named "second" of
          [first, second] -> do
            T.answer gate second "B"
            guarded view (T.settleHolding view (T.holds gate))
            T.textOf view ".answers" `shouldReturn` "B"
            T.answer gate first "A"
            guarded view (T.settleHolding view (T.holds gate))
            T.textOf view ".answers" `shouldReturn` "B,A"
            again <- failure (T.answer gate first "A")
            ("not held" `isInfixOf` again) `shouldBe` True
          _ -> fail "expected two asks"
        T.held gate >>= (`shouldBe` 0) . length
        guarded view (T.settle view)

    it "settles exactly between asks, a few thousand times over" $ do
      gate <- T.newGate
      T.withView id (asking gate) () $ \view -> do
        guarded view (T.settleHolding view (T.holds gate))
        T.held gate >>= mapM_ (\h -> T.answer gate h h.asked)
        guarded view (T.settleHolding view (T.holds gate))
        forM_ [1 .. 2000 :: Int] $ \i -> do
          T.click view "button.again"
          guarded view (T.settleHolding view (T.holds gate))
          T.held gate >>= \case
            [ask] -> T.answer gate ask ""
            asks -> fail ("round " <> show i <> ": " <> show (length asks) <> " asks held")
          guarded view (T.settleHolding view (T.holds gate))
          T.textOf view ".clicks" >>= \shown -> when (shown /= Text.pack (show i)) (fail ("round " <> show i <> ": " <> Text.unpack shown))

    it "lets two gates be waited on together" $ do
      one <- T.newGate
      other <- T.newGate
      T.withView id (asking one) () $ \view -> do
        guarded view (T.settleHolding view (T.holds one <> T.holds other))
        T.held one >>= (`shouldBe` 2) . length

  describe "control" $ do
    it "holds a view's waits" $ do
      T.withHeldWaits $ \waits ->
        T.withView id loading () $ \view -> do
          held <- guarded view (T.pending waits)
          T.textOf view ".loaded" `shouldReturn` "no"
          mapM_ T.release held
          guarded view (T.settle view)
          T.textOf view ".loaded" `shouldReturn` "yes"

    it "moves a view's pauses on the test's clock" $ do
      T.withTestClock $ \clock ->
        T.withView id loading () $ \view -> do
          guarded view (T.settle view)
          T.click view "button.tick"
          T.textOf view ".ticked" `shouldReturn` "no"
          T.advance clock 5
          guarded view (T.settle view)
          T.textOf view ".ticked" `shouldReturn` "yes"

  describe "twenty views side by side" $ do
    it "each hear only their own page's presses" $ do
      when rtsSupportsBoundThreads (setNumCapabilities 4)
      rung <- barrier 20
      pressed <- barrier 20
      outcomes <-
        inParallel
          [ T.withView id bell () $ \view -> do
              T.click view "button.bell"
              rung
              when (even i) (T.clickOutside view)
              pressed
              T.request view Report
          | i <- [0 .. 19 :: Int]
          ]
      -- The click on the bell is a press in the document too.
      outcomes `shouldBe` [if even i then (False, 2) else (True, 1) | i <- [0 .. 19 :: Int]]

#endif
