{-# LANGUAGE OverloadedLabels #-}

-- | What the gallery is around its examples: the navigation, a line about
-- the example that is showing, and the landing page. It is apart from
-- "Gallery", which mounts the examples, because it compiles natively: the
-- build renders it for every route ("Halogen.Static", @Prerender.hs@), and
-- each route's @index.html@ arrives with it already on the page.
--
-- Every example has an address of its own, a path under the site's base:
-- @\/@, @\/vanilla\/@, @\/hooks\/@ and so on (@\/haskell-halogen\/vanilla\/@ on
-- GitHub Pages). Each of them is a file on the server, written by the
-- prerender, so a reload or a link from elsewhere loads that route's page
-- straight away, with no fallback page to route a 404 through.
--
-- What a route's page holds before the program has loaded is this shell
-- with the route's line in it; the example itself, which needs the browser,
-- comes with the program, which renders the whole page afresh.
module Gallery.Shell
  ( Route (..)
  , route
  , examples
  , fromSlug
  , title
  , blurb
  , frame
  , landing
  , component
  )
where

import Clay qualified as C
import Clay.Flexbox qualified as Flex
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.Router (Follow, Router)
import Halogen.Router qualified as Router
import Halogen.Router.Duplex (RouteDuplex', noArgs, path, root)
import Halogen.Router.Duplex.Generic ((.=), (.|))
import Halogen.Router.Duplex.Generic qualified as Duplex
import Protolude

-- | Where the gallery is: its landing page, or one of the examples.
data Route = Home | Vanilla | Hooks | Pixi | Material | Quotes
  deriving stock (Eq, Show, Enum, Bounded, Generic)

-- | Each example's address ends in a slash, because each is a directory
-- with an @index.html@ in it.
route :: RouteDuplex' Route
route =
  root
    $ Duplex.sum
    $ #Home
      .= noArgs
      .| #Vanilla
        .= page Vanilla
      .| #Hooks
        .= page Hooks
      .| #Pixi
        .= page Pixi
      .| #Material
        .= page Material
      .| #Quotes
        .= page Quotes
  where
    page r = path (slug r <> "/") noArgs

examples :: [Route]
examples = [Vanilla .. maxBound]

-- | The name an example's address has: its directory, and what followed
-- @#\/@ when the gallery routed by fragment.
slug :: Route -> Text
slug = \case
  Home -> ""
  Vanilla -> "vanilla"
  Hooks -> "hooks"
  Pixi -> "pixi"
  Material -> "material"
  Quotes -> "th"

-- | The example a slug names, if any.
fromSlug :: Text -> Maybe Route
fromSlug s = find ((== s) . slug) examples

title :: Route -> Text
title = \case
  Home -> "haskell-halogen"
  Vanilla -> "Vanilla"
  Hooks -> "Hooks"
  Pixi -> "Pixi"
  Material -> "Material"
  Quotes -> "Quotes"

blurb :: Route -> Text
blurb = \case
  Home -> "Each one runs in this page, from the same WebAssembly binary. The back button works."
  Vanilla -> "A counter in the grid-bag layout, and a debounced child component."
  Hooks -> "Every hook in haskell-halogen-hooks, in one component."
  Pixi -> "A PixiJS canvas scene reconciled like any other HTML."
  Material -> "Material Components: tabs, lists, text fields, radios and checkboxes."
  Quotes -> "HTML and CSS written as such, with Haskell inside: the hh and clay quoters of haskell-halogen-th."

-- | The page around what a route shows: the navigation, with the current
-- route's line, and the area below it. Its links are the router's, so the
-- component they are in follows them ('Router.follow') with the action
-- @onFollow@ makes.
frame :: Router Route -> (Follow Route -> action) -> Route -> [HH.HTML w action] -> HH.HTML w action
frame router onFollow current content =
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
        ( map navLink (Home : examples)
            <> [ HH.span
                   [ HP.style $ do
                       C.marginLeft C.auto
                       C.fontSize (C.em 0.85)
                       C.opacity 0.7
                   ]
                   [HH.text (blurb current)]
               | current /= Home
               ]
        )
    , HH.main
        [ HP.style $ do
            C.flexGrow 1
            C.position C.relative
            C.overflow C.auto
        ]
        content
    ]
  where
    navLink target =
      Router.link
        router
        onFollow
        target
        [ HP.style $ do
            C.marginRight (C.px 20)
            C.textDecoration C.none
            C.color C.inherit
            when (current == target) $ C.fontWeight C.bold
            when (current /= target) $ C.opacity 0.7
        ]
        [HH.text (title target)]

-- | The landing page: what the gallery is, and a link to each example.
landing :: Router Route -> (Follow Route -> action) -> HH.HTML w action
landing router onFollow =
  HH.div
    [ HP.style $ do
        C.maxWidth (C.px 680)
        C.margin (C.px 0) C.auto (C.px 0) C.auto
        C.padding (C.px 32) (C.px 20) (C.px 32) (C.px 20)
    ]
    [ HH.h1_ [HH.text "haskell-halogen examples"]
    , HH.p_ [HH.text (blurb Home)]
    , HH.ul
        [HP.style $ C.paddingLeft (C.px 0) >> C.listStyleType C.none]
        [ HH.li
            [HP.style $ C.marginBottom (C.px 12)]
            [Router.link router onFollow e [] [HH.text (title e)], HH.text (" — " <> blurb e)]
        | e <- examples
        ]
    ]

-- | The shell alone, for a route: the landing page on the landing route,
-- and an empty area where an example would be on the others. This is what
-- the prerender renders.
component :: forall query output m. (MonadIO m) => Router Route -> H.Component query Route output m
component router =
  H.mkComponent $
    H.ComponentSpec
      { initialState = pure
      , render
      , eval = H.mkEval $ H.defaultEval {H.handleAction = handleAction, H.receive = Just . Arrived}
      }
  where
    render :: Route -> H.ComponentHTML Action Empty m
    render current = frame router Followed current [landing router Followed | current == Home]

    handleAction = \case
      Followed f -> Router.follow router f
      Arrived r -> put r

data Action = Followed (Follow Route) | Arrived Route
