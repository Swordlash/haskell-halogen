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
  )
where

import Data.Row (Empty, type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Router qualified as Router
import Protolude
import Web.Event.Event (EventType (..))
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
