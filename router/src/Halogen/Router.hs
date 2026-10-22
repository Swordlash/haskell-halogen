{-# LANGUAGE TemplateHaskell #-}

-- | Routing a page by its address: a route type, its codec
-- ("Halogen.Router.Duplex") and the browser's history, together.
--
-- A 'Router' is made once, when the application starts, and handed to the
-- components that route ('current' to know where they are, 'changes' to hear
-- when that changes, 'link' and 'navigate' to go somewhere):
--
-- > main = runBrowserDOM $ do
-- >   base <- Router.pageBase
-- >   router <- Router.newRouter base Home route
-- >   ...
--
-- Addresses are paths (@\/blog\/post\/1@), under a base path the site is
-- served from: a project's GitHub Pages site is under @\/\<project>\/@, and
-- every address the router writes starts with that, and every one it reads
-- has it taken off before its route is parsed. 'newHashRouter' keeps the
-- route in the fragment instead (@#\/blog\/post\/1@), for a page that is one
-- file a server knows nothing about.
--
-- 'changes' is one emitter per router, shared by everyone who subscribes:
-- it carries the router's own navigations and the browser's (back,
-- forward, a link it followed itself, an edited fragment), which arrive as
-- @popstate@. The @popstate@ listener is the router's for as long as the
-- page lives.
--
-- Natively there is no browser: the address is kept in memory, starting at
-- the base, and 'navigate' and 'redirect' change it there. That is enough
-- to render a page for a route ("Halogen.Static") or to test a component
-- that routes.
module Halogen.Router
  ( Router
  , newRouter
  , newHashRouter
  , pageBase

    -- * Where the page is
  , current
  , changes

    -- * Going somewhere
  , navigate
  , redirect
  , href
  , link
  , Follow (..)
  , follow
  , isPlainLeftClick
  )
where

import DOM.HTML.Indexed qualified as I
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText, inBrowser, toJSText)
import Halogen.Query.Event qualified as HQE
import Halogen.Router.Duplex (RouteDuplex', parse, print)
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, window, windowToEventTarget)
import Protolude hiding (link, print)
import UnliftIO (MonadUnliftIO)
import Web.Event.Event (Event (..), EventType (..), preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent (..), toEvent)

$( browserJS
     ["jsbits/router.js"]
     [ ("js_router_location", "js_router_location", Unsafe, [t|IO JSText|])
     , ("js_router_push_state", "js_router_push_state", Unsafe, [t|JSText -> IO ()|])
     , ("js_router_replace_state", "js_router_replace_state", Unsafe, [t|JSText -> IO ()|])
     , ("js_router_page_base", "js_router_page_base", Unsafe, [t|IO JSText|])
     , ("js_router_plain_left_click", "js_router_plain_left_click", Unsafe, [t|Event -> Bool|])
     ]
 )

-- | Where a router keeps the route in the address.
data Addressing
  = -- | In the path, under this base path (which starts and ends with @\/@).
    Paths Text
  | -- | In the fragment.
    Fragment

-- | The routes of a page of type @route@: how they are written in the
-- address, and the address itself.
data Router route = Router
  { duplex :: RouteDuplex' route
  , fallback :: route
  , addressing :: Addressing
  , memory :: IORef Text
  -- ^ The address natively, as a browser's @pathname + search + hash@; in a
  -- browser the address is the browser's.
  , notifier :: HS.Subscribe IO route
  }

-- | A router for addresses that are paths under a base path: @\"\/\"@ for a
-- site at the root of its host, @\"\/project\/\"@ for one under it (the
-- slashes around it are added if missing). The route codec should start with
-- 'Halogen.Router.Duplex.root'. An address the codec does not read, or one
-- that is not under the base, is the fallback route.
newRouter :: (MonadBrowserDOM m, MonadUnliftIO m) => Text -> route -> RouteDuplex' route -> m (Router route)
newRouter base = start (Paths (normaliseBase base)) (normaliseBase base)

-- | A router that keeps the route in the fragment: the codec's address
-- after a @#@, whatever the path is. Fragment links resolve against a
-- page's @\<base href>@, so a page with one should use 'newRouter' instead.
newHashRouter :: (MonadBrowserDOM m, MonadUnliftIO m) => route -> RouteDuplex' route -> m (Router route)
newHashRouter = start Fragment "/"

start :: (MonadBrowserDOM m, MonadUnliftIO m) => Addressing -> Text -> route -> RouteDuplex' route -> m (Router route)
start addressing initial fallback duplex = do
  memory <- liftIO (newIORef initial)
  notifier <- liftIO HS.create
  let router = Router {duplex, fallback, addressing, memory, notifier}
  target <- windowToEventTarget =<< window
  popstates <- HS.lowerEmitter (HQE.eventListener (EventType "popstate") target (const (Just ())))
  void $ liftIO $ HS.subscribe popstates $ \() -> current router >>= HS.notify notifier.listener
  pure router

-- | The base path a page's @\<base href>@ names, which is how a page served
-- under a path of its own can say what it is without the program being
-- built for it; @\"\/\"@ for a page without one, and natively.
pageBase :: (MonadIO m) => m Text
pageBase
  | inBrowser = liftIO $ normaliseBase . fromJSText <$> js_router_page_base
  | otherwise = pure "/"

normaliseBase :: Text -> Text
normaliseBase base = case T.dropAround (== '/') base of
  "" -> "/"
  inner -> "/" <> inner <> "/"

-- | The address as the browser has it: @pathname + search + hash@.
address :: Router route -> IO Text
address router
  | inBrowser = fromJSText <$> js_router_location
  | otherwise = readIORef router.memory

-- | The part of an address the codec reads, if the address is the router's.
routePart :: Addressing -> Text -> Maybe Text
routePart addressing whole = case addressing of
  Fragment -> Just $ case T.drop 1 (snd (T.breakOn "#" whole)) of
    "" -> "/"
    inner -> inner
  Paths base -> do
    after <- T.stripPrefix (T.dropWhileEnd (== '/') base) whole
    case T.uncons after of
      Nothing -> Just "/"
      Just ('/', _) -> Just after
      Just (c, _) | c `elem` ['?', '#'] -> Just ("/" <> after)
      _ -> Nothing

-- | The route the page is at now.
current :: (MonadIO m) => Router route -> m route
current router = liftIO $ do
  whole <- address router
  pure $ fromMaybe router.fallback (rightToMaybe . parse router.duplex =<< routePart router.addressing whole)

-- | Every route the page goes to from now on, by 'navigate' or 'redirect'
-- or by the browser's history: for 'Halogen.subscribe'.
changes :: Router route -> HS.Emitter IO route
changes router = router.notifier.emitter

-- | Where a link to the route points: the address 'navigate' goes to.
href :: Router route -> route -> Text
href router route = case router.addressing of
  Paths base -> T.dropWhileEnd (== '/') base <> absolute
  Fragment -> "#" <> absolute
  where
    printed = print router.duplex route
    absolute = if "/" `T.isPrefixOf` printed then printed else "/" <> printed

-- | Go to a route, as following a link to it does: a new entry in the
-- history, and the route on 'changes'. Going where the page already is
-- does nothing.
navigate :: (MonadIO m) => Router route -> route -> m ()
navigate router = go router js_router_push_state

-- | Go to a route in place of where the page is: the history entry is
-- replaced, so the back button skips the address being left. For an
-- address that only ever leads elsewhere, such as an old spelling of a
-- route.
redirect :: (MonadIO m) => Router route -> route -> m ()
redirect router = go router js_router_replace_state

go :: (MonadIO m) => Router route -> (JSText -> IO ()) -> route -> m ()
go router write route = liftIO $ do
  whole <- address router
  let target = href router route
      here = case router.addressing of
        Paths _ -> whole
        -- No fragment, or an empty one, is the root, as 'routePart' reads
        -- it: going to the root from there goes nowhere.
        Fragment -> case snd (T.breakOn "#" whole) of
          fragment | fragment `elem` ["", "#"] -> "#/"
          fragment -> fragment
  unless (target == here) $ do
    if inBrowser
      then write (toJSText target)
      else writeIORef router.memory $ case router.addressing of
        Paths _ -> target
        Fragment -> fst (T.breakOn "#" whole) <> target
    HS.notify router.notifier.listener route

-- | A click on a link to a route, to be followed: what 'link' hands its
-- component, and what 'follow' takes.
data Follow route = Follow route MouseEvent

-- | A link to a route, that a component follows itself, without loading a
-- page. Its @href@ is the route's address, so that it is still a link: to
-- open in a new tab, to copy, to follow before the program has loaded. A
-- plain click ('isPlainLeftClick') is handed to the component as an action
-- (@onFollow@), which passes it to 'follow'; any other click is the
-- browser's to follow as usual.
--
-- The browser is kept from following a plain click in the click's listener
-- itself ('HE.preventingDefault'), not by the action: the action runs when
-- the tree's loop gets to it, which, when the loop is busy as the link is
-- clicked (a render under way), is after the browser has finished
-- dispatching the click and loaded the page.
--
-- The link's @href@ and its click handler are the router's: they come
-- after @props@, and of two properties of the same name an element keeps
-- the last, so an @href@ or a click handler among @props@ is overridden.
link
  :: forall route w action
   . Router route
  -> (Follow route -> action)
  -> route
  -> [HP.IProp I.HTMLa action]
  -> [HH.HTML w action]
  -> HH.HTML w action
link router onFollow route props =
  HH.a (props <> [HP.href (href router route), HE.preventingDefault (HE.handler' (EventType "click") clicked)])
  where
    clicked ev = if isPlainLeftClick (MouseEvent ev) then Just (onFollow (Follow route (MouseEvent ev))) else Nothing

-- | Follow a link's click: 'navigate', at once, in 'H.liftEffect'.
--
-- The click from a 'link' has had its default prevented already, in its
-- listener. This prevents it too, for a 'Follow' made by hand from a click
-- handler of one's own, but that is only in time if the tree's loop was free
-- when the click arrived: such a handler should be wrapped in
-- 'HE.preventingDefault' as 'link''s is.
follow
  :: forall route state action slots output m
   . (MonadIO m)
  => Router route
  -> Follow route
  -> H.HalogenM state action slots output m ()
follow router (Follow route ev) = H.liftEffect $ liftIO $ do
  preventDefault (toEvent ev)
  navigate router route

-- | A click with the main button and no modifier key, on a link that opens
-- in its own tab: the one a page follows itself. Natively never.
isPlainLeftClick :: MouseEvent -> Bool
isPlainLeftClick = js_router_plain_left_click . toEvent
