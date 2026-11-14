{-# LANGUAGE TemplateHaskell #-}

-- | Routing a page by its address: a route type, its codec
-- ("Halogen.Router.Duplex") and the browser's history, together.
--
-- A 'Router' is made once, when the application starts, and handed to the
-- components that route ('current' to know where they are, 'changes' to hear
-- when that changes, 'link' and 'navigate' to go somewhere, or 'linkProps'
-- for a link written as HTML):
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
-- file a server knows nothing about, or for a part of a page whose path is
-- someone else's.
--
-- 'changes' is one emitter per router, shared by everyone who subscribes:
-- it carries the router's own navigations and the browser's (back,
-- forward, a link it followed itself, an edited fragment), which arrive as
-- @popstate@. The @popstate@ listener is the router's for as long as the
-- page lives, unless the router is 'stop'ped, as a program that runs again
-- on the same page (browser GHCi's @:main@) stops the last run's router.
--
-- Natively there is no browser, but there may be a page: the router asks
-- the window its DOM monad gives ('Halogen.VDom.DOM.Monad.window') for its
-- @location@ and @history@, through core's @Web.HTML.Location@ and
-- @Web.HTML.History@. A window that answers for itself
-- ("Halogen.JSBits.Value"'s @withNativeObject@) gives them: the session of
-- haskell-halogen-remote, whose page is across a wire, keeps a mirror of the
-- page's address and has the page push and replace its entries, so a
-- router made in its monad reads where the page is, writes the page's
-- address, and hears its back and forward as in a browser. A window that
-- does not ('Halogen.VDom.DOM.Monad.MemDOM''s) leaves the address in
-- memory, starting at the base, and 'navigate' and 'redirect' change it
-- there. That is enough to render a page for a route ("Halogen.Static") or
-- to test a component that routes.
module Halogen.Router
  ( Router
  , newRouter
  , newHashRouter
  , pageBase
  , stop

    -- * Where the page is
  , current
  , changes

    -- * Going somewhere
  , navigate
  , redirect
  , href
  , fragmentAddress
  , link
  , linkProps
  , Follow (..)
  , follow
  , isPlainLeftClick
  )
where

import DOM.HTML.Indexed qualified as I
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, fromJSText, inBrowser, jsNull, nativeGet, toJSText)
import Halogen.Query.Event qualified as HQE
import Halogen.Router.Duplex (RouteDuplex', parse, print)
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.HandlerOptions qualified as HandlerOptions
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, document, window, windowToEventTarget)
import Protolude hiding (Location, link, print)
import UnliftIO (MonadUnliftIO)
import Web.DOM.Internal.Types (Window (..))
import Web.Event.Event (EventType (..), preventDefault)
import Web.HTML.History (History (..))
import Web.HTML.History qualified as History
import Web.HTML.Location (Location (..))
import Web.HTML.Location qualified as Location
import Web.UIEvent.MouseEvent (MouseEvent (..), toEvent)

$( browserJS
     ["jsbits/router.js"]
     [ ("js_router_location", "js_router_location", Safe, [t|IO JSText|])
     , ("js_router_push_state", "js_router_push_state", Safe, [t|JSText -> IO ()|])
     , ("js_router_replace_state", "js_router_replace_state", Safe, [t|JSText -> IO ()|])
     , ("js_router_page_base", "js_router_page_base", Safe, [t|IO JSText|])
     ]
 )

-- | Where a router keeps the route in the address.
data Addressing
  = -- | In the path, under this base path (which starts and ends with @\/@).
    Paths Text
  | -- | In the fragment.
    Fragment

-- | Where the address is.
data Address
  = -- | The browser's: the router's own bindings.
    Browser
  | -- | A page's, natively, through a window that answers for itself: its
    -- location and its history.
    Answered Location History
  | -- | In memory, natively, as a browser's @pathname + search + hash@.
    Memory (IORef Text)

-- | The routes of a page of type @route@: how they are written in the
-- address, and the address itself.
data Router route = Router
  { duplex :: RouteDuplex' route
  , fallback :: route
  , addressing :: Addressing
  , address :: Address
  , page :: Text
  -- ^ The address the page was at when the router was made, without its
  -- fragment: what a hash router's 'href' writes before the @#@.
  , notifier :: HS.Subscribe IO route
  , popstate :: HS.Subscription IO
  -- ^ The router's @popstate@ listener, which 'stop' takes away.
  }

-- | A router for addresses that are paths under a base path: @\"\/\"@ for a
-- site at the root of its host, @\"\/project\/\"@ for one under it (the
-- slashes around it are added if missing). The route codec should start with
-- 'Halogen.Router.Duplex.root'. An address the codec does not read, or one
-- that is not under the base, is the fallback route.
newRouter :: (MonadBrowserDOM m, MonadUnliftIO m) => Text -> route -> RouteDuplex' route -> m (Router route)
newRouter base = start (Paths (normaliseBase base)) (normaliseBase base)

-- | A router that keeps the route in the fragment: the codec's address
-- after a @#@, whatever the path is. It leaves the path and the query alone,
-- so it can live in a page whose path another router reads.
--
-- Its addresses are written in full, the page's own path and query before
-- the @#@ (@\/lesson\/?q=1#\/about@), and not as a bare @#\/about@: a bare
-- fragment resolves against the page's @\<base href>@ when it has one, which
-- names another address than the page's (@\/#\/about@ under a base of
-- @\/@), so a link would leave the page and a push would change its path.
-- 'navigate' and 'redirect' write the path the page is at as they go, so
-- they keep it when something else changed it since the router was made; a
-- link's @href@, which is a pure value, names the path the page was at when
-- the router was made. In a page whose path changes while a hash router
-- lives, a plain click on its link goes where it should and a link opened
-- in a new tab goes to the router's first path: make a router for each path
-- if that matters.
newHashRouter :: (MonadBrowserDOM m, MonadUnliftIO m) => route -> RouteDuplex' route -> m (Router route)
newHashRouter = start Fragment "/"

start :: (MonadBrowserDOM m, MonadUnliftIO m) => Addressing -> Text -> route -> RouteDuplex' route -> m (Router route)
start addressing initial fallback duplex = do
  w <- window
  address <- liftIO (addressOf w initial)
  page <- liftIO (fst . T.breakOn "#" <$> readAddress address)
  notifier <- liftIO HS.create
  target <- windowToEventTarget w
  popstates <- HS.lowerEmitter (HQE.eventListener (EventType "popstate") target (const (Just ())))
  -- The listener reads where the page is through a router whose
  -- subscription is not yet known: 'current' reads only the address.
  let reading = Router {duplex, fallback, addressing, address, page, notifier, popstate = HS.Subscription pass}
  listening <- liftIO $ HS.subscribe popstates $ \() -> current reading >>= HS.notify notifier.listener
  -- Taking the listener off twice would free its callback twice, so only
  -- the first 'stop' does.
  stopped <- liftIO (newIORef False)
  let popstate = HS.Subscription $ do
        already <- atomicModifyIORef' stopped (True,)
        unless already (HS.unsubscribe listening)
  pure reading {popstate}

-- | Stop following the browser's history: the router's @popstate@ listener
-- is taken off the window, so a back or forward the browser goes no longer
-- reaches 'changes'. Its own 'navigate' and 'redirect' still do. This is for
-- a page whose program is run again on it, as browser GHCi's @:main@ runs
-- it: the last run's router stops with the last run's tree (see
-- "Halogen.IO.Util".'Halogen.IO.Util.keepForNextRun') rather than go on
-- listening beside the next run's. Stopping a stopped router does nothing.
stop :: (MonadIO m) => Router route -> m ()
stop router = liftIO (HS.unsubscribe router.popstate)

-- | Where the address is kept, given the window: the browser's in a
-- browser; natively, the page's if the window answers for its location and
-- its history, and otherwise memory, starting at the address given.
addressOf :: Window -> Text -> IO Address
addressOf (Window w) initial
  | inBrowser = pure Browser
  | Just location <- nativeGet held "location"
  , Just history <- nativeGet held "history" =
      pure (Answered (Location location) (History history))
  | otherwise = Memory <$> newIORef initial
  where
    held = w :: JSVal

-- | The base path a page's @\<base href>@ names, which is how a page served
-- under a path of its own can say what it is without the program being
-- built for it; @\"\/\"@ for a page without one.
--
-- Natively it is the path of the document's @baseURI@, if the window's
-- document answers for it (a page across a wire, whose session is told the
-- address of its @\<base href>@ and answers with that when the page has
-- one), and @\"\/\"@ otherwise.
pageBase :: (MonadBrowserDOM m, MonadIO m) => m Text
pageBase
  | inBrowser = liftIO $ normaliseBase . fromJSText <$> js_router_page_base
  | otherwise = do
      doc <- document =<< window
      liftIO $ normaliseBase . pathOf <$> doc.baseURI

-- | The path of an address: what comes after its scheme and host (for one
-- that has them), up to its query or fragment.
pathOf :: Text -> Text
pathOf whole = T.takeWhile (`notElem` ['?', '#']) afterHost
  where
    afterHost = case T.breakOn "://" whole of
      (_, "") -> whole
      (_, rest) -> T.dropWhile (/= '/') (T.drop 3 rest)

normaliseBase :: Text -> Text
normaliseBase base = case T.dropAround (== '/') base of
  "" -> "/"
  inner -> "/" <> inner <> "/"

-- | The address as the browser has it: @pathname + search + hash@.
readAddress :: Address -> IO Text
readAddress = \case
  Browser -> fromJSText <$> js_router_location
  Answered location _ -> do
    path <- Location.pathname location
    query <- Location.search location
    fragment <- Location.hash location
    pure (path <> query <> fragment)
  Memory memory -> readIORef memory

-- | A new entry in the history, or the current one replaced.
data Write = Push | Replace

writeAddress :: Address -> Write -> Text -> IO ()
writeAddress address write target = case address of
  Browser -> case write of
    Push -> js_router_push_state (toJSText target)
    Replace -> js_router_replace_state (toJSText target)
  Answered _ history -> case write of
    Push -> History.pushState history jsNull "" (Just target)
    Replace -> History.replaceState history jsNull "" (Just target)
  Memory memory -> writeIORef memory target

-- | The address a hash router writes for a fragment, on a page at an
-- address (@pathname + search + hash@): the page's own path and query, then
-- the fragment, which replaces the page's.
--
-- > fragmentAddress "/lesson/?q=1#/old" "#/about" == "/lesson/?q=1#/about"
fragmentAddress :: Text -> Text -> Text
fragmentAddress whole fragment = fst (T.breakOn "#" whole) <> fragment

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
  whole <- readAddress router.address
  pure $ fromMaybe router.fallback (rightToMaybe . parse router.duplex =<< routePart router.addressing whole)

-- | Every route the page goes to from now on, by 'navigate' or 'redirect'
-- or by the browser's history: for 'Halogen.subscribe'.
changes :: Router route -> HS.Emitter IO route
changes router = router.notifier.emitter

-- | Where a link to the route points: the address 'navigate' goes to.
href :: Router route -> route -> Text
href router route = case router.addressing of
  Paths base -> T.dropWhileEnd (== '/') base <> absolute
  Fragment -> fragmentAddress router.page ("#" <> absolute)
  where
    absolute = absoluteRoute router route

-- | The codec's address for a route, starting with @/@.
absoluteRoute :: Router route -> route -> Text
absoluteRoute router route = if "/" `T.isPrefixOf` printed then printed else "/" <> printed
  where
    printed = print router.duplex route

-- | Go to a route, as following a link to it does: a new entry in the
-- history, and the route on 'changes'. Going where the page already is
-- does nothing.
navigate :: (MonadIO m) => Router route -> route -> m ()
navigate router = go router Push

-- | Go to a route in place of where the page is: the history entry is
-- replaced, so the back button skips the address being left. For an
-- address that only ever leads elsewhere, such as an old spelling of a
-- route.
redirect :: (MonadIO m) => Router route -> route -> m ()
redirect router = go router Replace

go :: (MonadIO m) => Router route -> Write -> route -> m ()
go router write route = liftIO $ do
  whole <- readAddress router.address
  -- A hash router compares fragments, and writes the path the page is at
  -- now rather than the one 'href' names, so as not to take the page back
  -- to a path something else has left since the router was made.
  let (target, there, here) = case router.addressing of
        Paths _ -> (href router route, href router route, whole)
        Fragment ->
          let fragment = "#" <> absoluteRoute router route
           in ( fragmentAddress whole fragment
              , fragment
              , -- No fragment, or an empty one, is the root, as 'routePart'
                -- reads it: going to the root from there goes nowhere.
                case snd (T.breakOn "#" whole) of
                  at | at `elem` ["", "#"] -> "#/"
                  at -> at
              )
  unless (there == here) $ do
    writeAddress router.address write target
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
--
-- It is 'HH.a' with 'linkProps' after @props@.
link
  :: forall route w action
   . Router route
  -> (Follow route -> action)
  -> route
  -> [HP.IProp I.HTMLa action]
  -> [HH.HTML w action]
  -> HH.HTML w action
link router onFollow route props = HH.a (props <> linkProps router onFollow route)

-- | What makes an @\<a>@ a link to a route, as 'link' makes it: the
-- route's @href@ and the click handler that hands a plain click to the
-- component (@onFollow@), with the default prevented in the listener. The
-- handler declares that it is for plain clicks ('HE.matching'
-- 'HE.plainLeftClick'), so that a page that is not the program's (a
-- component served by haskell-halogen-remote) prevents the default of the
-- same clicks, and of no other, while it dispatches them. For a
-- link whose element is written some other way, such as in an @hh@ quote
-- of "Halogen.TH":
--
-- > <a class="nav" {{ Router.linkProps router Followed (Post 1) }}>The first post</a>
--
-- Of two properties of the same name an element keeps the last, so an
-- @href@ or a click handler written after these overrides the router's:
-- write them last, as 'link' puts them after its @props@.
linkProps
  :: forall route action
   . Router route
  -> (Follow route -> action)
  -> route
  -> [HP.IProp I.HTMLa action]
linkProps router onFollow route =
  [ HP.href (href router route)
  , HE.preventingDefault (HE.matching HE.plainLeftClick (HE.handler (EventType "click") (onFollow . Follow route . MouseEvent)))
  ]

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

-- | A click with the main button and no modifier key, that nothing has
-- prevented already, on a link that opens in its own tab and is not a
-- download: the one a page follows itself. It is what 'linkProps' declares
-- its handler matches ('HE.plainLeftClick'), and
-- "Halogen.VDom.DOM.HandlerOptions".'Halogen.VDom.DOM.HandlerOptions.isPlainLeftClick'
-- says how it is read, in a browser and natively from the event's JSON.
isPlainLeftClick :: MouseEvent -> Bool
isPlainLeftClick = HandlerOptions.isPlainLeftClick . toEvent
