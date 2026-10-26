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
  )
where

import Data.Row (Empty, type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude

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
