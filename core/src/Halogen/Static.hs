-- | A component rendered to HTML, with no browser: the page a server sends,
-- or a build writes out, ahead of the program that will run it.
--
-- This is full server-side rendering, not a printout of the first render. The
-- component is mounted, with the driver that runs it in a browser, into an
-- in-memory document ("Halogen.VDom.DOM.Monad.Native"): its children are
-- built, its slots filled, and its @Initialize@ and its children's run to the
-- end, 'Halogen.liftIO' and all, before the HTML is taken. So a page can be
-- rendered with what a component fetches or reads when it starts. What it
-- leaves for later is not waited for: a fiber it forks, a subscription's next
-- event, a timer. Once the HTML is taken the tree is disposed of: its
-- subscriptions end, its fibers are cancelled and its finalizers run, so
-- nothing it started outlives the render. A finalizer's
-- 'Halogen.liftEffect' runs before 'renderToHtml' returns; what it waits for
-- in 'Halogen.liftIO' is started but not waited for, as with any disposal.
--
-- A component that does browser things when it starts, such as measuring an
-- element or reading the URL, asks 'Halogen.JSBits.inBrowser' first and
-- leaves them out here; the browser does them when the page runs. The
-- in-memory document answers the DOM's questions as an empty page would.
--
-- How properties become markup, and what cannot, is in
-- 'Halogen.VDom.DOM.Monad.Native.renderToText'.
--
-- The page this makes is static until its program has loaded. The program
-- then mounts the component with 'Halogen.VDom.Driver.hydrateUI', which
-- adopts the HTML rather than render the component afresh in its place.
--
-- Native only: on the browser backends there is a real document to render
-- into instead.
module Halogen.Static
  ( renderToHtml
  , renderToHtmlWith
  , fillMountPoint
  )
where

import Data.Text qualified as T
import HPrelude
import Halogen.Component (Component)
import Halogen.IO.Driver (HalogenSocket (..))
import Halogen.IO.Driver qualified as AD
import Halogen.VDom.DOM.Monad (MemDOM, MonadBrowserDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver (renderSpec)
import Halogen.VDom.Types (ElemName (..))

-- | The HTML a component renders for this input, once it has initialized.
--
-- The HTML is the component's own, without a container around it: what goes
-- inside the element the program will mount it into.
renderToHtml :: forall query input output. Component query input output MemDOM -> input -> IO Text
renderToHtml = renderToHtmlWith runMemDOM

-- | 'renderToHtml' for a component whose monad is an application's own, such
-- as @ReaderT Config MemDOM@, given how to run it.
--
-- Natively every 'MonadBrowserDOM' is the in-memory one, possibly under
-- transformers, which is what makes the document this renders into the one
-- the component speaks.
renderToHtmlWith
  :: forall m query input output
   . (MonadBrowserDOM m, MonadUnliftIO m)
  => (forall a. m a -> IO a)
  -> Component query input output m
  -> input
  -> IO Text
renderToHtmlWith run component input = do
  document <- N.newDocument
  container <- N.newElement Nothing (ElemName "div")
  HalogenSocket {dispose} <- run $ AD.runUI (renderSpec (N.fromNative document) (N.fromNative container)) component input
  -- Taken before disposing of the tree, which removes its HTML from the
  -- container; disposed of even if taking it fails.
  (mconcat <$> (traverse N.renderToText =<< N.childNodes container)) `finally` run dispose

-- | Put a component's HTML into the page it will be mounted in: into the
-- page's empty @\<div id=\"…\">\<\/div>@ with this id, written exactly so.
-- 'Left' says why when the page has no such element, or more than one: the
-- page is a template a build fills, and one it cannot fill is a mistake to
-- report rather than a page to ship without its content.
fillMountPoint :: Text -> Text -> Text -> Either Text Text
fillMountPoint ident html page = case T.splitOn mountPoint page of
  [before, after] -> Right (before <> open <> html <> close <> after)
  [_] -> Left ("the page has no empty mount point " <> mountPoint)
  _ -> Left ("the page has more than one mount point " <> mountPoint)
  where
    open = "<div id=\"" <> ident <> "\">"
    close = "</div>"
    mountPoint = open <> close
