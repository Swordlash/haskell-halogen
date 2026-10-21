-- | Every example on one page, one at a time.
--
-- Which one is showing lives in the URL's path, @\/pixi\/@ and so on under
-- the site's base, read and written by a 'Router' ("Halogen.Router"): a link
-- in the navigation goes there without loading a page, and the back and
-- forward buttons, a reload and a link from elsewhere all arrive at the
-- right example. The page around the examples is "Gallery.Shell", which the
-- build also renders into each route's @index.html@.
--
-- The gallery used to route by fragment, @#\/pixi@; such a link, from
-- before, is redirected to its path when the gallery starts.
--
-- Switching examples removes the previous one's slot, so this page also puts
-- their finalizers to work: a subscription or a timer an example forgot to
-- release would outlive it here.
--
-- Only the browser backends compile this module: the Pixi example is written
-- against 'BrowserDOM' itself, which on a host GHC has no DOM to speak.
module Gallery (component) where

import Data.Row (type (.+), type (.==))
import Data.Text qualified as T
import Example.Hooks qualified as Hooks
import Example.Material qualified as Material
import Example.Pixi qualified as Pixi
import Example.Quotes qualified as Quotes
import Example.Vanilla qualified as Vanilla
import Gallery.Shell (Route (..))
import Gallery.Shell qualified as Shell
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Router (Follow, Router)
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (BrowserDOM, window)
import Protolude
import Web.HTML.Window qualified as Window

data Action = Initialize | Arrived Route | Followed (Follow Route)

type Slots =
  ("vanilla" .== H.Slot Vanilla.Query Int ())
    .+ ("hooks" .== H.Slot H.VoidF Hooks.Output ())
    .+ ("pixi" .== H.Slot H.VoidF Void ())
    .+ ("material" .== H.Slot H.VoidF () ())
    .+ ("th" .== H.Slot H.VoidF () ())

component :: H.Component H.VoidF (Router Route) Void BrowserDOM
component =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \router -> (router,) <$> Router.current router
      , render
      , eval = H.mkEval $ H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize}
      }
  where
    render :: (Router Route, Route) -> H.ComponentHTML Action Slots BrowserDOM
    render (router, current) = Shell.frame router Followed current [view router current]

    view router = \case
      Home -> Shell.landing router Followed
      Vanilla -> HH.slot_ "vanilla" () Vanilla.component ()
      Hooks -> HH.slot_ "hooks" () Hooks.app ()
      Pixi -> HH.slot_ "pixi" () Pixi.parent ()
      Material -> HH.slot_ "material" () Material.component ()
      Quotes -> HH.slot_ "th" () Quotes.component ()

    handleAction = \case
      Initialize -> do
        router <- gets fst
        void $ H.subscribe (Arrived <$> Router.changes router)
        -- A link from when the gallery routed by fragment: #/pixi is /pixi/.
        fragment <- lift (Window.locationHash =<< window)
        for_ (T.stripPrefix "#/" fragment >>= Shell.fromSlug) (Router.redirect router)
      Arrived r -> modify (second (const r))
      Followed f -> do
        router <- gets fst
        Router.follow router f
