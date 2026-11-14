{-# LANGUAGE OverloadedLabels #-}

-- | Components the suites send over a wire: ordinary components,
-- polymorphic in their monad, that know nothing of it.
module Test.Components
  ( CounterQuery (..)
  , counter
  , EchoQuery (..)
  , echo
  , toggling
  , swapping
  , form
  , guardedForm
  , pointing
  , linking
  , searching
  , failing
  , operating
  , hearing
  , titling
  , Spot (..)
  , spots
  , routing
  , hashRouting
  , storing
  )
where

import Control.Monad.UUID (MonadUUID)
import Data.Row (Empty, (.+), type (.==))
import Data.Row qualified as Row
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Router qualified as Router
import Halogen.Router.Duplex (RouteDuplex', int, noArgs, path, root, segment)
import Halogen.Router.Duplex.Generic qualified as RG
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Halogen.VDom.DOM.Monad qualified as DOM
import Protolude
import UnliftIO (MonadUnliftIO)
import Web.DOM.Element qualified as Element
import Web.DOM.Internal.Types (Element)
import Web.Event.Event (EventType (..))
import Web.HTML.Common (AttrName (..))
import Web.HTML.HTMLDialogElement qualified as HTMLDialogElement
import Web.HTML.HTMLElement qualified as HTMLElement
import Web.Storage.Storage qualified as Storage
import Web.UIEvent.KeyboardEvent.EventTypes qualified as KET
import Web.UIEvent.MouseEvent qualified as ME

data CounterQuery a = GetCount (Int -> a)

data Count = Increment

-- | A button that counts its clicks, and the count beside it.
counter :: forall m. (Monad m) => H.Component CounterQuery () Void m
counter =
  H.component
    (\() -> pure (0 :: Int))
    render
    [ H.onAction (\Increment -> modify (+ 1))
    , H.onQuery (\(GetCount reply) -> Just . reply <$> get)
    ]
  where
    render :: Int -> H.ComponentHTML Count Empty m
    render n =
      HH.div_
        [ HH.button [HP.id "increment", HE.onClick (const Increment)] [HH.text "+"]
        , HH.span [HP.id "count"] [HH.text (show n)]
        ]

data EchoQuery a = GetText (Text -> a)

-- | An input whose text is shown under it as it is typed.
echo :: forall m. (Monad m) => H.Component EchoQuery () Void m
echo =
  H.component
    (\() -> pure ("" :: Text))
    render
    [ H.onAction put
    , H.onQuery (\(GetText reply) -> Just . reply <$> get)
    ]
  where
    render :: Text -> H.ComponentHTML Text Empty m
    render typed =
      HH.div_
        [ HH.input [HP.id "field", HE.onValueInput identity]
        , HH.p [HP.id "echo"] [HH.text typed]
        ]

data Toggle = Toggle

type Slots = ("child" .== H.Slot H.VoidF Void ())

-- | A button that shows and hides a child, which has a button of its own.
toggling :: forall m. (Monad m) => H.Component H.VoidF () Void m
toggling =
  H.component
    (\() -> pure True)
    render
    [H.onAction (\Toggle -> modify not)]
  where
    render :: Bool -> H.ComponentHTML Toggle Slots m
    render shown =
      HH.div_
        [ HH.button [HP.id "toggle", HE.onClick (const Toggle)] [HH.text "toggle"]
        , if shown then HH.slot_ "child" () poker () else HH.text "hidden"
        ]

-- | A button that counts its pokes: the child of 'toggling' and 'swapping'.
poker :: forall m. (Monad m) => H.Component H.VoidF () Void m
poker =
  H.component
    (\() -> pure (0 :: Int))
    (\n -> HH.button [HP.id "poke", HE.onClick (const ())] [HH.text ("poked " <> show n)] :: H.ComponentHTML () Empty m)
    [H.onAction (\() -> modify (+ 1))]

data Swap = Swap

-- | A child in an element whose tag the parent's state picks. Swapping the
-- tag replaces the element: the old one is taken out with the child's node
-- still in it, and the driver, which keeps the child, puts that same node
-- into the new element in the same render.
swapping :: forall m. (Monad m) => H.Component H.VoidF () Void m
swapping =
  H.component
    (\() -> pure False)
    render
    [H.onAction (\Swap -> modify not)]
  where
    render :: Bool -> H.ComponentHTML Swap Slots m
    render inSection =
      HH.div_
        [ HH.button [HP.id "swap", HE.onClick (const Swap)] [HH.text "swap"]
        , if inSection
            then HH.section [HP.id "wrapper"] [HH.slot_ "child" () poker ()]
            else HH.div [HP.id "wrapper"] [HH.slot_ "child" () poker ()]
        ]

data Submitted = Submitted

-- | A form whose submission the component counts, which the page must not
-- carry out itself (a navigation would end the test's page).
form :: forall m. (Monad m) => H.Component H.VoidF () Void m
form =
  H.component
    (\() -> pure (0 :: Int))
    render
    [H.onAction (\Submitted -> modify (+ 1))]
  where
    render :: Int -> H.ComponentHTML Submitted Empty m
    render n =
      HH.form
        [HP.id "form", HP.action "/elsewhere", HE.onSubmit (const Submitted)]
        [ HH.button [HP.id "submit"] [HH.text "send"]
        , HH.span [HP.id "submitted"] [HH.text (show n)]
        ]

-- | 'form', its handler declared with 'HE.preventingDefault' and
-- 'HE.stoppingPropagation', which the listener acts on, on the event the
-- backend hands it, before the action.
guardedForm :: forall m. (Monad m) => H.Component H.VoidF () Void m
guardedForm =
  H.component
    (\() -> pure (0 :: Int))
    render
    [H.onAction (\Submitted -> modify (+ 1))]
  where
    render :: Int -> H.ComponentHTML Submitted Empty m
    render n =
      HH.form
        [HP.id "form", HP.action "/elsewhere", HE.stoppingPropagation (HE.preventingDefault (HE.onSubmit (const Submitted)))]
        [ HH.button [HP.id "submit"] [HH.text "send"]
        , HH.span [HP.id "submitted"] [HH.text (show n)]
        ]

-- | Where the last click on its button was, as the click's handler read it.
pointing :: forall m. (Monad m) => H.Component H.VoidF () Void m
pointing =
  H.component
    (\() -> pure (Nothing :: Maybe (Double, Double)))
    render
    [H.onAction (put . Just)]
  where
    render :: Maybe (Double, Double) -> H.ComponentHTML (Double, Double) Empty m
    render at =
      HH.div_
        [ HH.button [HP.id "point", HE.onClick (\e -> (ME.clientX e, ME.clientY e))] [HH.text "here"]
        , HH.span [HP.id "where"] [HH.text (maybe "nowhere" (\(x, y) -> show x <> "," <> show y) at)]
        ]

data Followed = Followed

-- | A link whose plain clicks the component counts, decided in the click's
-- listener from the event's fields ('Router.isPlainLeftClick'), with the
-- default prevented there, as "Halogen.Router"'s links are: a click with a
-- modifier key, or on a link that opens elsewhere, is no action.
linking :: forall m. (Monad m) => H.Component H.VoidF () Void m
linking =
  H.component
    (\() -> pure (0 :: Int))
    render
    [H.onAction (\Followed -> modify (+ 1))]
  where
    render :: Int -> H.ComponentHTML Followed Empty m
    render n =
      HH.div_
        [ HH.a
            [ HP.id "link"
            , HP.href "/elsewhere"
            , HE.preventingDefault (HE.handler' (EventType "click") (\e -> if Router.isPlainLeftClick (ME.MouseEvent e) then Just Followed else Nothing))
            ]
            [HH.text "follow"]
        , HH.span [HP.id "followed"] [HH.text (show n)]
        ]

data Search = Searched | Swapped

-- | A field in a box whose keydown handler is for one key, @/@ (or, once
-- swapped, @Escape@), declared ('HE.matching') with its default prevented:
-- the key is not typed into the field, every other key is, and is not the
-- handler's. It counts the keys it was handed.
searching :: forall m. (Monad m) => H.Component H.VoidF () Void m
searching =
  H.component
    (\() -> pure (0 :: Int, "/"))
    render
    [ H.onAction $ \case
        Searched -> modify (first (+ 1))
        Swapped -> modify (second (const "Escape"))
    ]
  where
    render :: (Int, Text) -> H.ComponentHTML Search Empty m
    render (n, key) =
      HH.div_
        [ HH.div
            [ HP.id "search"
            , HE.preventingDefault (HE.matching (HE.keys [key]) (HE.handler KET.keydown (const Searched)))
            ]
            [HH.input [HP.id "query"]]
        , HH.span [HP.id "searched"] [HH.text (show n)]
        , HH.button [HP.id "swap-key", HE.onClick (const Swapped)] [HH.text "swap"]
        ]

-- | A component that cannot start: its initial state throws.
failing :: forall m. (MonadIO m) => H.Component H.VoidF () Void m
failing =
  H.component
    (\() -> liftIO (throwIO (ErrorCall "no state to start from")) :: m ())
    (\() -> HH.text "never shown" :: H.ComponentHTML () Empty m)
    []

data Operations = Operations
  { first :: H.RefLabel
  , second :: H.RefLabel
  , dialog :: H.RefLabel
  , far :: H.RefLabel
  , doomed :: H.RefLabel
  , kept :: Maybe Element
  -- ^ The element under @doomed@, kept once it is gone.
  , shown :: Bool
  }

data Operate
  = FocusSecond
  | FocusQuietly
  | OpenDialog
  | CloseDialog
  | ScrollFar
  | Forget
  | FocusForgotten
  | NotListed

-- | What a component does to the page through the @Web.*@ types, through
-- refs, as a component written for a page does it: it focuses its first
-- field in its @Initialize@, and on its buttons focuses the second field
-- (with and without scrolling), opens and closes a dialog, scrolls an
-- element far down the page into view, takes away a field after keeping
-- its element and then focuses that element, and sets an attribute
-- through the element (a method the page across a wire is not asked to
-- call).
operating :: forall m. (MonadIO m, MonadUUID m) => H.Component H.VoidF () Void m
operating =
  H.component
    ( \() ->
        Operations
          <$> H.newRefLabel "first"
          <*> H.newRefLabel "second"
          <*> H.newRefLabel "dialog"
          <*> H.newRefLabel "far"
          <*> H.newRefLabel "doomed"
          <*> pure Nothing
          <*> pure True
    )
    render
    [ H.onInitialize (focusing (.first) HTMLElement.focus)
    , H.onAction $ \case
        FocusSecond -> focusing (.second) HTMLElement.focus
        FocusQuietly -> focusing (.second) HTMLElement.focusWithoutScrolling
        OpenDialog -> withDialog (.showModal)
        CloseDialog -> withDialog HTMLDialogElement.close
        ScrollFar -> do
          s <- get
          H.getRef s.far >>= traverse_ (\e -> H.liftEffect (liftIO (Element.scrollIntoViewBlock e Element.ToCenter)))
        Forget -> do
          s <- get
          element <- H.getRef s.doomed
          modify (\st -> st {kept = element, shown = False})
        FocusForgotten -> do
          s <- get
          for_ (s.kept >>= HTMLElement.fromElement) (H.liftEffect . liftIO . HTMLElement.focus)
        NotListed -> do
          s <- get
          H.getRef s.second >>= traverse_ (\e -> H.liftEffect (liftIO (e.setAttribute "data-set" "by the program")))
    ]
  where
    focusing label act = do
      s <- get
      H.getHTMLElementRef (label s) >>= traverse_ (H.liftEffect . liftIO . act)
    withDialog act = do
      s <- get
      dialog <- (>>= HTMLDialogElement.fromElement) <$> H.getRef s.dialog
      for_ dialog (H.liftEffect . liftIO . act)
    render :: Operations -> H.ComponentHTML Operate Empty m
    render s =
      HH.div_ $
        [ HH.input [HP.id "first", HP.ref s.first]
        , HH.input [HP.id "second", HP.ref s.second]
        , HH.button [HP.id "focus-second", HE.onClick (const FocusSecond)] [HH.text "Focus"]
        , HH.button [HP.id "focus-quietly", HE.onClick (const FocusQuietly)] [HH.text "Focus quietly"]
        , HH.button [HP.id "open", HE.onClick (const OpenDialog)] [HH.text "Open"]
        , HH.dialog
            [HP.id "dialog", HP.ref s.dialog]
            [HH.p_ [HH.text "In a dialog"], HH.button [HP.id "close", HE.onClick (const CloseDialog)] [HH.text "Close"]]
        , HH.button [HP.id "scroll", HE.onClick (const ScrollFar)] [HH.text "Scroll"]
        , HH.button [HP.id "forget", HE.onClick (const Forget)] [HH.text "Forget"]
        , HH.button [HP.id "focus-forgotten", HE.onClick (const FocusForgotten)] [HH.text "Focus the forgotten"]
        , HH.button [HP.id "not-listed", HE.onClick (const NotListed)] [HH.text "Set an attribute"]
        ]
          <> [ HH.div [HP.id "spacer", HP.styleText "height: 4000px"] []
             , HH.div [HP.id "far", HP.ref s.far] [HH.text "Far down"]
             ]
          <> [HH.input [HP.id "doomed", HP.ref s.doomed] | s.shown]

data Hearing = Hearing
  { field :: H.RefLabel
  , dialog :: H.RefLabel
  , heard :: [Text]
  }

data Hear
  = FocusField
  | CloseDialogHeard
  | Heard Text

-- | A component that listens to what it does to the page: a button focuses
-- a field whose @focus@ it hears, and another closes a dialog (shown open)
-- whose @close@ it hears. What it heard is in @#heard@. Hearing takes it a
-- while, so that a test that reads the page before the event is handled
-- reads what was there before.
hearing :: forall m. (MonadIO m, MonadUUID m) => H.Component H.VoidF () Void m
hearing =
  H.component
    (\() -> Hearing <$> H.newRefLabel "field" <*> H.newRefLabel "dialog" <*> pure [])
    render
    [ H.onAction $ \case
        FocusField -> do
          s <- get
          H.getHTMLElementRef s.field >>= traverse_ (H.liftEffect . liftIO . HTMLElement.focus)
        CloseDialogHeard -> do
          s <- get
          dialog <- (>>= HTMLDialogElement.fromElement) <$> H.getRef s.dialog
          for_ dialog (H.liftEffect . liftIO . HTMLDialogElement.close)
        Heard what -> do
          H.liftEffect (liftIO (threadDelay 50000))
          modify (\s -> s {heard = s.heard <> [what]})
    ]
  where
    render :: Hearing -> H.ComponentHTML Hear Empty m
    render s =
      HH.div_
        [ HH.input [HP.id "heard-field", HP.ref s.field, HE.onFocus (const (Heard "focus"))]
        , HH.button [HP.id "focus-field", HE.onClick (const FocusField)] [HH.text "Focus"]
        , HH.button [HP.id "close-heard", HE.onClick (const CloseDialogHeard)] [HH.text "Close"]
        , HH.dialog
            [HP.ref s.dialog, HP.attr (AttrName "open") "", HE.handler (EventType "close") (const (Heard "close"))]
            [HH.text "Open from the start"]
        , HH.p [HP.id "heard"] [HH.text (T.intercalate ", " s.heard)]
        ]

data Retitle = Retitle Text

-- | A component that sets the page's title in its @Initialize@, through the
-- document the backend gives, and again on a click, through the window's
-- @document@ field: the two ways a component reaches the title.
titling :: forall m. (MonadIO m, MonadBrowserDOM m) => H.Component H.VoidF () Void m
titling =
  H.component
    (\() -> pure ())
    (\() -> HH.button [HP.id "retitle", HE.onClick (const (Retitle "Second title"))] [HH.text "Retitle"] :: H.ComponentHTML Retitle Empty m)
    [ H.onInitialize $ do
        doc <- H.liftEffect (DOM.window >>= DOM.document)
        H.liftEffect (liftIO (doc.title_set "First title"))
    , H.onAction $ \(Retitle t) -> do
        w <- H.liftEffect DOM.window
        H.liftEffect (liftIO (w.document.title_set t))
    ]

-- | Three places of a site: its front page, a post, and a page about it.
data Spot = Front | Post Int | About
  deriving stock (Eq, Show, Generic)

spots :: RouteDuplex' Spot
spots =
  root
    $ RG.sum
    $ #Front Row..== noArgs
      .+ #Post Row..== path "blog" (path "post" (int segment))
      .+ #About Row..== path "about" noArgs

data Moving = Moved Spot | Went (Router.Follow Spot) | RedirectAbout

-- | Where the router says the page is, links to each place, and a button
-- that redirects to the page about the site (the history's entry
-- replaced): what a routed page of an application is, given its router.
routing :: forall q m. (MonadIO m) => H.Component q (Router.Router Spot) Void m
routing = routed pure (const pass)

-- | 'routing', with a hash router of its own, made as it starts and stopped
-- as it goes: for a page whose address only its fragment should change.
hashRouting :: forall q m. (MonadUnliftIO m, MonadBrowserDOM m) => H.Component q () Void m
hashRouting = routed (\() -> Router.newHashRouter Front spots) Router.stop

routed :: forall q i m. (MonadIO m) => (i -> m (Router.Router Spot)) -> (Router.Router Spot -> IO ()) -> H.Component q i Void m
routed made stopped =
  H.component
    (\i -> made i >>= \router -> (router,) <$> Router.current router)
    render
    [ H.onInitialize $ do
        (router, _) <- get
        void (H.subscribe (Moved <$> Router.changes router))
    , H.onFinalize $ do
        (router, _) <- get
        liftIO (stopped router)
    , H.onAction $ \case
        Moved spot -> modify (\(router, _) -> (router, spot))
        Went f -> do
          (router, _) <- get
          Router.follow router f
        RedirectAbout -> do
          (router, _) <- get
          Router.redirect router About
    ]
  where
    render :: (Router.Router Spot, Spot) -> H.ComponentHTML Moving Empty m
    render (router, spot) =
      HH.div_
        [ HH.h1 [HP.id "spot"] [HH.text (show spot)]
        , Router.link router Went Front [HP.id "to-front"] [HH.text "Front"]
        , Router.link router Went (Post 1) [HP.id "to-post"] [HH.text "The first post"]
        , Router.link router Went About [HP.id "to-about"] [HH.text "About"]
        , HH.button [HP.id "redirect", HE.onClick (const RedirectAbout)] [HH.text "Redirect"]
        ]

data Stored = Stored {count :: Int, visits :: Int}

data Storing = Bump | Visit | ForgetCount

-- | A count kept in local storage and one kept in session storage, through
-- "Web.Storage.Storage", read as it starts and written on each click.
storing :: forall q m. (MonadBrowserDOM m) => H.Component q () Void m
storing =
  H.component
    (\() -> Stored <$> stored Storage.LocalStorage "count" <*> stored Storage.SessionStorage "visits")
    render
    [ H.onAction $ \case
        Bump -> do
          modify (\s -> s {count = s.count + 1})
          s <- get
          H.liftEffect (Storage.setItem Storage.LocalStorage "count" s.count)
        Visit -> do
          modify (\s -> s {visits = s.visits + 1})
          s <- get
          H.liftEffect (Storage.setItem Storage.SessionStorage "visits" s.visits)
        ForgetCount -> do
          modify (\s -> s {count = 0})
          H.liftEffect (Storage.removeItem Storage.LocalStorage "count")
    ]
  where
    stored :: Storage.StorageKind -> Text -> m Int
    stored kind key = fromMaybe 0 . (>>= rightToMaybe) <$> Storage.getItem kind key
    render :: Stored -> H.ComponentHTML Storing Empty m
    render s =
      HH.div_
        [ HH.span [HP.id "count"] [HH.text (show s.count)]
        , HH.span [HP.id "visits"] [HH.text (show s.visits)]
        , HH.button [HP.id "bump", HE.onClick (const Bump)] [HH.text "Bump"]
        , HH.button [HP.id "visit", HE.onClick (const Visit)] [HH.text "Visit"]
        , HH.button [HP.id "forget-count", HE.onClick (const ForgetCount)] [HH.text "Forget"]
        ]
