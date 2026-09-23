-- | Every example on one page, one at a time.
--
-- Which one is showing lives in the URL's fragment, @#/pixi@ and so on, rather
-- than in the component's state alone. Following a link to a fragment adds a
-- history entry without loading a page, so the back and forward buttons, a
-- reload and a link from elsewhere all arrive at the right example, and a
-- static file server has only this one page to serve. The component reads the
-- fragment when it starts and again on every @hashchange@.
--
-- Switching examples removes the previous one's slot, so this page also puts
-- their finalizers to work: a subscription or a timer an example forgot to
-- release would outlive it here.
--
-- Only the browser backends compile this module: the Pixi example is written
-- against 'BrowserDOM' itself, which on a host GHC has no DOM to speak.
module Gallery (component) where

import Clay qualified as C
import Clay.Flexbox qualified as Flex
import Data.Row (type (.+), type (.==))
import Data.Text qualified as T
import Example.Hooks qualified as Hooks
import Example.Material qualified as Material
import Example.Pixi qualified as Pixi
import Example.Vanilla qualified as Vanilla
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Query.Event qualified as HQE
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (BrowserDOM, window, windowToEventTarget)
import Protolude
import Web.Event.Event (EventType (..))
import Web.HTML.Window qualified as Window

data Example = Vanilla | Hooks | Pixi | Material
  deriving stock (Eq, Enum, Bounded)

examples :: [Example]
examples = [minBound .. maxBound]

-- | The fragment an example lives at, after the @#/@.
slug :: Example -> Text
slug = \case
  Vanilla -> "vanilla"
  Hooks -> "hooks"
  Pixi -> "pixi"
  Material -> "material"

title :: Example -> Text
title = \case
  Vanilla -> "Vanilla"
  Hooks -> "Hooks"
  Pixi -> "Pixi"
  Material -> "Material"

blurb :: Example -> Text
blurb = \case
  Vanilla -> "A counter in the grid-bag layout, and a debounced child component."
  Hooks -> "Every hook in haskell-halogen-hooks, in one component."
  Pixi -> "A PixiJS canvas scene reconciled like any other HTML."
  Material -> "Material Components: tabs, lists, text fields, radios and checkboxes."

-- | Nothing is the landing page: no fragment, @#/@, or one that names no
-- example.
fromHash :: Text -> Maybe Example
fromHash fragment = find ((== path) . slug) examples
  where
    path = T.dropWhile (== '/') (T.dropWhile (== '#') fragment)

href :: Maybe Example -> Text
href = ("#/" <>) . maybe "" slug

data Action = Initialize | HashChanged

type Slots =
  ("vanilla" .== H.Slot Vanilla.Query Int ())
    .+ ("hooks" .== H.Slot H.VoidF Hooks.Output ())
    .+ ("pixi" .== H.Slot H.VoidF Void ())
    .+ ("material" .== H.Slot H.VoidF () ())

component :: H.Component H.VoidF () Void BrowserDOM
component =
  H.mkComponent $
    H.ComponentSpec
      { initialState = const (pure Nothing)
      , render
      , eval = H.mkEval $ H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize}
      }
  where
    render :: Maybe Example -> H.ComponentHTML Action Slots BrowserDOM
    render current =
      HH.div
        [ HP.style $ do
            C.display C.flex
            C.flexDirection C.column
            C.height (C.vh 100)
            C.fontFamily ["system-ui"] [C.sansSerif]
        ]
        [ HH.nav
            [ HP.style $ do
                C.display C.flex
                C.flexWrap Flex.wrap
                C.alignItems C.center
                C.padding (C.px 12) (C.px 20) (C.px 12) (C.px 20)
                C.borderBottom (C.px 1) C.solid (C.rgb 0xd0 0xd0 0xd0)
            ]
            (navLink current Nothing "haskell-halogen" : map (\e -> navLink current (Just e) (title e)) examples)
        , HH.main
            [ HP.style $ do
                C.flexGrow 1
                C.position C.relative
                C.overflow C.auto
            ]
            [maybe landing view current]
        ]

    navLink current target label =
      HH.a
        [ HP.href (href target)
        , HP.style $ do
            C.marginRight (C.px 20)
            C.textDecoration C.none
            C.color C.inherit
            when (current == target) $ C.fontWeight C.bold
            when (current /= target) $ C.opacity 0.7
        ]
        [HH.text label]

    landing =
      HH.div
        [ HP.style $ do
            C.maxWidth (C.px 680)
            C.margin (C.px 0) C.auto (C.px 0) C.auto
            C.padding (C.px 32) (C.px 20) (C.px 32) (C.px 20)
        ]
        [ HH.h1_ [HH.text "haskell-halogen examples"]
        , HH.p_ [HH.text "Each one runs in this page, from the same WebAssembly binary. The back button works."]
        , HH.ul
            [HP.style $ C.paddingLeft (C.px 0) >> C.listStyleType C.none]
            [ HH.li [HP.style $ C.marginBottom (C.px 12)] [HH.a [HP.href (href (Just e))] [HH.text (title e)], HH.text (" — " <> blurb e)]
            | e <- examples
            ]
        ]

    view = \case
      Vanilla -> HH.slot_ "vanilla" () Vanilla.component ()
      Hooks -> HH.slot_ "hooks" () Hooks.app ()
      Pixi -> HH.slot_ "pixi" () Pixi.parent ()
      Material -> HH.slot_ "material" () Material.component ()

    handleAction = \case
      Initialize -> do
        target <- lift (windowToEventTarget =<< window)
        changes <- lift $ HS.lowerEmitter $ HQE.eventListener (EventType "hashchange") target (const (Just HashChanged))
        void $ H.subscribe changes
        handleAction HashChanged
      HashChanged -> do
        fragment <- lift (Window.locationHash =<< window)
        put (fromHash fragment)
