-- | The whole of a LiveView at once: "Halogen.Remote.Server" natively,
-- Chromium driven by "Halogen.Playwright", the real @remote.js@ and a real
-- WebSocket. Each test reads both sides: the page through the driver, and
-- the component's side through what the test shares with it in this
-- process (a 'TVar' its handlers write) and "Halogen.Activity".
--
-- The browser reaches the server through a small TCP proxy the test can
-- cut, which is how a connection is lost from the server's side.
--
-- The second half is the package's way of testing a component, and its
-- acceptance test: "Halogen.Remote.Test" mounts a component per page in
-- this process, and the driver settles through the page's probe, which the
-- session answers over the wire. Those examples read the page once, with
-- no retry: what they read is there when the action returns, or the probe
-- lied.
module Main (main) where

import Control.Concurrent
  ( forkIO
  , killThread
  , threadDelay
  )
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM hiding (check)
import Control.Concurrent.STM qualified as STM
import Control.Exception (bracket, finally, try)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (get, modify)
import Data.ByteString qualified as ByteString
import Data.Functor (($>))
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Clock (getMonotonicTime)
import Halogen qualified as H
import Halogen.Activity qualified as Activity
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Remote.DOM (RemoteDOM)
import Halogen.Remote.Server qualified as Server
import Halogen.Remote.Session qualified as Session
import Halogen.Remote.Test (Mounted (..), mounting, mountingEach, queryComponent, withTestServer)
import Halogen.Remote.Test qualified as RemoteTest
import Halogen.Router qualified as Router
import Halogen.Router.Duplex (RouteDuplex', lmap, noArgs, optional, prefix, root)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket
import Network.Wai.Handler.Warp qualified as Warp
import Prelude
import System.Timeout (timeout)
import Test.Components (Spot (..), hearing, operating, routing, spots, storing, titling)
import Web.UIEvent.KeyboardEvent.EventTypes qualified as KET

-- | What the sessions' components tell the test.
data Shared = Shared
  { nextSession :: TVar Int
  , counts :: TVar (Map Int Int)
  -- ^ Each live session's count.
  , typed :: TVar (Map Int Text)
  , finalized :: TVar [Int]
  , follows :: TVar Int
  -- ^ How many link clicks the components have followed, in every session.
  , searches :: TVar Int
  -- ^ How many keys the search box's handler was handed, in every session.
  }

newShared :: IO Shared
newShared = Shared <$> newTVarIO 1 <*> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO [] <*> newTVarIO 0 <*> newTVarIO 0

data Place = Home | Other
  deriving stock (Eq, Show)

routes :: RouteDuplex' Place
routes = root (lmap toOther (maybe Home (const Other) <$> optional (prefix "other" noArgs)))
  where
    toOther = \case
      Home -> Nothing
      Other -> Just ()

data State = State
  { session :: Int
  , count :: Int
  , typedText :: Text
  , router :: Router.Router Place
  , route :: Place
  }

data Action = Increment | Typed Text | Follow (Router.Follow Place) | Search

-- | A counter, an input and a router's link, each session numbered: what
-- its handlers do they also write where the test can see it. The router is
-- the session's, made natively, over the page's address as the page said
-- it first: a link it follows changes what the page shows and the page's
-- address, with no page loaded.
live :: Shared -> H.Component H.VoidF () Void RemoteM
live shared =
  H.component
    ( \() -> do
        n <- liftIO (atomically (stateTVar shared.nextSession (\n -> (n, n + 1))))
        router <- Router.newRouter "/" Home routes
        pure State {session = n, count = 0, typedText = "", router, route = Home}
    )
    render
    [ H.onInitialize $ do
        s <- get
        H.liftEffect (liftIO (atomically (modifyTVar' shared.counts (Map.insert s.session 0))))
    , H.onAction $ \case
        Increment -> do
          modify (\s -> s {count = s.count + 1})
          s <- get
          H.liftEffect (liftIO (atomically (modifyTVar' shared.counts (Map.insert s.session s.count))))
        Typed t -> do
          modify (\s -> s {typedText = t})
          s <- get
          H.liftEffect (liftIO (atomically (modifyTVar' shared.typed (Map.insert s.session t))))
        Follow f -> do
          s <- get
          Router.follow s.router f
          place <- Router.current s.router
          modify (\st -> st {route = place})
          H.liftEffect (liftIO (atomically (modifyTVar' shared.follows (+ 1))))
        Search -> H.liftEffect (liftIO (atomically (modifyTVar' shared.searches (+ 1))))
    , H.onFinalize $ do
        s <- get
        H.liftEffect $
          liftIO $
            atomically $ do
              modifyTVar' shared.counts (Map.delete s.session)
              modifyTVar' shared.finalized (s.session :)
    ]
  where
    render :: State -> H.ComponentHTML Action Empty RemoteM
    render s =
      HH.div_
        [ HH.span [HP.id "session"] [HH.text (Text.pack (show s.session))]
        , HH.button [HP.id "increment", HE.onClick (const Increment)] [HH.text "+"]
        , HH.span [HP.id "count"] [HH.text (Text.pack (show s.count))]
        , HH.input [HP.id "name", HE.onValueInput Typed]
        , HH.p [HP.id "echo"] [HH.text s.typedText]
        , Router.link s.router Follow Other [HP.id "to-other"] [HH.text "elsewhere"]
        , HH.span [HP.id "route"] [HH.text (Text.pack (show s.route))]
        , -- A box whose keydown handler is for "/", which it keeps from
          -- being typed into the field in it: declared, so that the page
          -- can prevent it, and send no other key.
          HH.div
            [HP.id "search", HE.preventingDefault (HE.matching (HE.keys ["/"]) (HE.handler KET.keydown (const Search)))]
            [HH.input [HP.id "query"]]
        ]

type RemoteM = RemoteDOM

-- | Wait, up to five seconds, for what the components wrote to say so.
until' :: String -> STM Bool -> IO ()
until' what condition =
  timeout 5_000_000 (atomically (condition >>= STM.check)) >>= \case
    Just () -> pure ()
    Nothing -> expectationFailure ("the server never saw " <> what)

main :: IO ()
main = do
  shared <- newShared
  let settings = Server.defaultSettings {Server.pages = [("/", "test/web/live.html", "text/html")]}
  Warp.testWithApplication (pure (Server.application settings (live shared) ())) $ \serverPort ->
    withProxy serverPort $ \proxy -> withTestServer $ \server -> hspec (spec shared proxy >> testing server)

spec :: Shared -> Proxy -> Spec
spec shared proxy = playwright $ describe "a component served to Chromium" $ do
  let base = Text.pack ("http://127.0.0.1:" <> show proxy.port <> "/")
      sessionOf page = read . Text.unpack <$> eventually page (textOf page "#session")
      textOf page css = do
        t <- textContent (locator page css)
        when (Text.null t) $ expectationFailure ("nothing in " <> Text.unpack css <> " yet")
        pure t

  it "renders, and the page's clicks and typing reach the component" $ inPage $ \page -> do
    earlier <- Activity.activity
    goto page base
    session <- sessionOf page
    locator page "#count" `shouldHaveText` "0"
    Activity.activity >>= \a -> a.trees `shouldBe` earlier.trees + 1
    click (locator page "#increment")
    -- The component's side first: its handler ran, and the tree has done
    -- all it does about it (rendered and sent the patch).
    until' "the click" ((== Just 1) . Map.lookup session <$> readTVar shared.counts)
    Activity.waitIdle
    -- The patch still crosses the wire, which no program in this page
    -- answers for: the expectation retries.
    locator page "#count" `shouldHaveText` "1"
    typeText (locator page "#name") "Ada"
    until' "the text" ((== Just "Ada") . Map.lookup session <$> readTVar shared.typed)
    Activity.waitIdle
    locator page "#echo" `shouldHaveText` "Ada"

  it "follows a router's link on a plain click, and leaves any other click to the browser" $ inPage $ \page -> do
    goto page base
    session <- sessionOf page
    address <- url page
    -- A listener of the test's on the document, which hears a click on the
    -- link after the client's listener there: it notes whether the default was
    -- prevented by then, and prevents it itself, so that a click the
    -- browser would have followed elsewhere (a new tab) goes nowhere.
    True <-
      evaluate page $
        Text.unlines
          [ "(() => {"
          , "  window.prevented = []; window.stayed = true;"
          , "  document.addEventListener('click', (e) => {"
          , "    if (e.target.closest('#to-other')) { window.prevented.push(e.defaultPrevented); e.preventDefault(); }"
          , "  });"
          , "  return true;"
          , "})()"
          ]
    before <- readTVarIO shared.follows
    -- A ctrl-click and a meta-click, which ask for a new tab: the client
    -- leaves them to the browser and the link's handler makes no action.
    True <-
      evaluate page $
        Text.unlines
          [ "(() => {"
          , "  const link = document.querySelector('#to-other');"
          , "  for (const key of ['ctrlKey', 'metaKey'])"
          , "    link.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, button: 0, [key]: true }));"
          , "  return true;"
          , "})()"
          ]
    evaluate page "window.prevented" `shouldReturn` [False, False]
    -- Messages cross in order: once a later click's action has run, the
    -- two before it have been dealt with.
    click (locator page "#increment")
    until' "the click after them" ((== Just 1) . Map.lookup session <$> readTVar shared.counts)
    readTVarIO shared.follows `shouldReturn` before
    locator page "#route" `shouldHaveText` "Home"
    -- The link's handler declares that it is for plain clicks, so the two
    -- did not even cross the wire: the one click sent is the button's.
    let clicksSent = length . filter (\f -> f.sent && "\"type\":\"click\"" `Text.isInfixOf` f.payload) <$> webSocketFrames page
    eventually page $ clicksSent `shouldReturn` 1
    -- A plain click: prevented in the page, followed by the component.
    click (locator page "#to-other")
    until' "the link followed" ((== before + 1) <$> readTVar shared.follows)
    locator page "#route" `shouldHaveText` "Other"
    evaluate page "window.prevented" `shouldReturn` [False, False, True]
    -- No page was loaded, though the address is the route's: what the test
    -- put on the window is still there.
    url page `shouldReturn` (address <> "other")
    evaluate page "window.stayed === true" `shouldReturn` True

  it "keeps a key a handler declared from being typed, and sends the program no other" $ inPage $ \page -> do
    goto page base
    session <- sessionOf page
    before <- readTVarIO shared.searches
    typeText (locator page "#query") "a/b"
    until' "the slash" ((== before + 1) <$> readTVar shared.searches)
    -- The page prevented the slash's default, while it dispatched the
    -- key: it was not typed. The other keys were, as the handler is not
    -- for them.
    locator page "#query" `shouldHaveValue` "ab"
    -- A click after the keys, once the page has sent it, is after them on
    -- the wire: of the three keys, only the slash crossed it.
    click (locator page "#increment")
    until' "the click after them" ((== Just 1) . Map.lookup session <$> readTVar shared.counts)
    let sentWith text seen = [f | f <- seen, f.sent, text `Text.isInfixOf` f.payload]
    eventually page $ (length . sentWith "\"type\":\"click\"" <$> webSocketFrames page) `shouldReturn` 1
    (length . sentWith "\"type\":\"keydown\"" <$> webSocketFrames page) `shouldReturn` 1
    readTVarIO shared.searches `shouldReturn` (before + 1)

  it "gives two tabs two sessions, each with its own state" $ \pw ->
    withContext pw defaultContextOptions {baseURL = Just base} $ \ctx -> do
      one <- newPage ctx
      two <- newPage ctx
      goto one "/"
      goto two "/"
      first' <- sessionOf one
      second' <- sessionOf two
      first' `shouldNotBe` second'
      click (locator one "#increment")
      click (locator one "#increment")
      click (locator two "#increment")
      until' "both tabs' clicks" $ do
        cs <- readTVar shared.counts
        pure (Map.lookup first' cs == Just 2 && Map.lookup second' cs == Just 1)
      locator one "#count" `shouldHaveText` "2"
      locator two "#count" `shouldHaveText` "1"

  it "finalizes a closed tab's component on the server" $ \pw ->
    withContext pw defaultContextOptions {baseURL = Just base} $ \ctx -> do
      page <- newPage ctx
      goto page "/"
      session <- sessionOf page
      earlier <- Activity.activity
      closePage page
      until' "the session end" (elem session <$> readTVar shared.finalized)
      Activity.waitIdle
      Activity.activity >>= \a -> a.trees `shouldBe` earlier.trees - 1

  it "reconnects when the server's side drops the connection, as a new session" $ inPage $ \page -> do
    goto page base
    session <- sessionOf page
    click (locator page "#increment")
    locator page "#count" `shouldHaveText` "1"
    cut proxy
    until' "the lost connection end its session" (elem session <$> readTVar shared.finalized)
    -- The client reconnects by itself, after its delay: a new session,
    -- rendered from the start.
    eventually page $ do
      again <- sessionOf page
      again `shouldSatisfy` (> session)
    locator page "#count" `shouldHaveText` "0"
    locator page "#app" `shouldHaveAttribute` "data-halogen-remote" $ Just "connected"

----------------------------------------------------------------------
-- A component per page, through Halogen.Remote.Test.

data ViewState = ViewState
  { label :: Text
  , notice :: Bool
  , bumps :: Int
  }

data ViewAction = Compute | Fetch | Save | Busy | Bump

data ViewQuery a = SetLabel Text a

-- | A view whose handlers take time in each of the ways a program does:
-- work on the loop ('H.liftEffect', which holds it), a wait in 'liftIO'
-- (a request), a pause ('H.delay', a notice that goes away by itself).
-- Its initial state takes a moment too, and its @Initialize@ changes what
-- the first render showed.
view :: forall m. (MonadIO m) => H.Component ViewQuery () Void m
view =
  H.component
    (\() -> liftIO (threadDelay 300_000) $> ViewState {label = "loading", notice = False, bumps = 0})
    render
    [ H.onInitialize (modify (\s -> s {label = "ready"}))
    , H.onAction $ \case
        Compute -> do
          H.liftEffect (liftIO (threadDelay 300_000))
          modify (\s -> s {label = "computed"})
        Fetch -> do
          liftIO (threadDelay 700_000)
          modify (\s -> s {label = "fetched"})
        Save -> do
          modify (\s -> s {notice = True})
          void (H.fork (H.delay 3 >> modify (\s -> s {notice = False})))
        Busy -> do
          liftIO (threadDelay 6_000_000)
          modify (\s -> s {label = "rested"})
        Bump -> modify (\s -> s {bumps = s.bumps + 1})
    , H.onQuery $ \case
        SetLabel t next -> modify (\s -> s {label = t}) $> Just next
    ]
  where
    render :: ViewState -> H.ComponentHTML ViewAction Empty m
    render s =
      HH.div_
        [ HH.span [HP.id "label"] [HH.text s.label]
        , HH.button [HP.id "compute", HE.onClick (const Compute)] [HH.text "Compute"]
        , HH.button [HP.id "fetch", HE.onClick (const Fetch)] [HH.text "Fetch"]
        , HH.button [HP.id "save", HE.onClick (const Save)] [HH.text "Save"]
        , HH.button [HP.id "busy", HE.onClick (const Busy)] [HH.text "Rest"]
        , HH.button [HP.id "bump", HE.onClick (const Bump)] [HH.text "Bump"]
        , HH.span [HP.id "bumps"] [HH.text (Text.pack (show s.bumps))]
        , HH.span [HP.id "notice"] [HH.text (if s.notice then "Saved" else "")]
        ]

-- | How long an action took, in seconds.
timed :: IO a -> IO (a, Double)
timed action = do
  start <- getMonotonicTime
  a <- action
  end <- getMonotonicTime
  pure (a, end - start)

testing :: RemoteTest.TestServer -> Spec
testing server = playwright $ describe "a component per page, mounted in the test's process (Halogen.Remote.Test)" $ do
  let readOnce page css = textContent (locator page css)

  it "returns from visit once the first render, and what Initialize changed, is on the page" $ inPage $ \page -> do
    (address, connected) <- mounting server view ()
    visit page address
    readOnce page "#label" `shouldReturn` "ready"
    mounted <- connected
    Session.activity mounted.session >>= \a -> a.trees `shouldBe` 1

  it "settles a click once the work it caused on the loop is rendered and sent" $ inPage $ \page -> do
    (address, _) <- mounting server view ()
    visit page address
    click (byRoleNamed page "button" "Compute")
    readOnce page "#label" `shouldReturn` "computed"

  it "waits, in waitQuiet, for a wait in liftIO and what it rendered" $ inPage $ \page -> do
    (address, _) <- mounting server view ()
    visit page address
    click (locator page "#fetch")
    -- The click settles while the request is still out: idle, not quiet.
    readOnce page "#label" `shouldReturn` "ready"
    activity page >>= \a -> fmap (.waiting) a `shouldBe` Just 1
    waitQuiet page
    readOnce page "#label" `shouldReturn` "fetched"

  it "does not wait for a pause (H.delay): the notice is read while it shows" $ inPage $ \page -> do
    (address, _) <- mounting server view ()
    visit page address
    click (locator page "#save")
    readOnce page "#notice" `shouldReturn` "Saved"
    ((), took) <- timed (waitQuiet page)
    took `shouldSatisfy` (< 2)
    readOnce page "#notice" `shouldReturn` "Saved"

  it "settles each page by its own session: a busy one does not hold up another" $ \pw ->
    withContext pw defaultContextOptions $ \ctx -> do
      busy <- newPage ctx
      calm <- newPage ctx
      (busyAddress, _) <- mounting server view ()
      (calmAddress, _) <- mounting server view ()
      _ <- concurrently (visit busy busyAddress) (visit calm calmAddress)
      -- Six seconds in a liftIO, on the busy page's session.
      click (locator busy "#busy")
      ((), took) <- timed $ do
        click (locator calm "#bump")
        click (locator calm "#bump")
        waitQuiet calm
      readOnce calm "#bumps" `shouldReturn` "2"
      took `shouldSatisfy` (< 2)
      -- The process as a whole is not quiet: a probe that answered for
      -- every tree of it would have kept the calm page waiting for the
      -- busy one's six seconds.
      processWide <- Activity.activity
      processWide.waiting `shouldSatisfy` (>= 1)
      activity busy >>= \a -> fmap (.waiting) a `shouldBe` Just 1
      activity calm >>= \a -> fmap (.waiting) a `shouldBe` Just 0

  it "stays disconnected once the server ends the session: no reconnection" $ inPage $ \page -> do
    (address, connected) <- mounting server view ()
    visit page address
    mounted <- connected
    locator page "#halogen-remote-test" `shouldHaveAttribute` "data-halogen-remote" $ Just "connected"
    Session.dispose mounted.session
    locator page "#halogen-remote-test" `shouldHaveAttribute` "data-halogen-remote" $ Just "disconnected"
    -- Longer than the client's first delay before it would reconnect.
    threadDelay 1_500_000
    getAttribute (locator page "#halogen-remote-test") "data-halogen-remote" `shouldReturn` Just "disconnected"
    -- With no connection, the page's probe says there is nothing.
    activity page `shouldReturn` Just Activity {trees = 0, running = 0, waiting = 0}
    -- What it showed stays on the page.
    readOnce page "#label" `shouldReturn` "ready"

  -- What the component does to the page through the Web types, carried
  -- out by the client: each read once, after the action settles.
  it "gives the focus to a field the component focuses, in Initialize and after a click" $ inPage $ \page -> do
    (address, _) <- mounting server operating ()
    visit page address
    evaluate page "document.activeElement && document.activeElement.id" `shouldReturn` ("first" :: Text)
    click (locator page "#focus-second")
    evaluate page "document.activeElement && document.activeElement.id" `shouldReturn` ("second" :: Text)

  -- An event the page sends because of what the program did to it, after
  -- the driver's question: the click settles only once it is handled too.
  it "waits for the component to hear the focus it asked for, and a dialog's close" $ inPage $ \page -> do
    (address, _) <- mounting server hearing ()
    visit page address
    click (locator page "#focus-field")
    readOnce page "#heard" `shouldReturn` "focus"
    click (locator page "#close-heard")
    readOnce page "#heard" `shouldReturn` "focus, close"

  it "opens a dialog as a modal with showModal, and closes it with close" $ inPage $ \page -> do
    (address, _) <- mounting server operating ()
    visit page address
    let dialog = "(() => { const d = document.querySelector('#dialog'); return [d.open, d.matches(':modal')]; })()"
    evaluate page dialog `shouldReturn` [False, False]
    click (locator page "#open")
    evaluate page dialog `shouldReturn` [True, True]
    click (locator page "#close")
    evaluate page dialog `shouldReturn` [False, False]

  it "scrolls an element far down the page into view" $ inPage $ \page -> do
    (address, _) <- mounting server operating ()
    visit page address
    let inView = "(() => { const r = document.querySelector('#far').getBoundingClientRect(); return r.top >= 0 && r.bottom <= window.innerHeight; })()"
    evaluate page inView `shouldReturn` False
    click (locator page "#scroll")
    evaluate page inView `shouldReturn` True

  it "carries out no method the client does not allow" $ inPage $ \page -> do
    (address, _) <- mounting server operating ()
    visit page address
    click (locator page "#not-listed")
    getAttribute (locator page "#second") "data-set" `shouldReturn` Nothing

  it "sets the page's title as the component sets it, and again when it sets another" $ inPage $ \page -> do
    (address, _) <- mounting server titling ()
    visit page address
    evaluate page "document.title" `shouldReturn` ("First title" :: Text)
    click (locator page "#retitle")
    evaluate page "document.title" `shouldReturn` ("Second title" :: Text)

  -- The page's address and stores, which the session mirrors: a router
  -- made in the session's monad, as an application's main makes it, and a
  -- component that keeps counts through Web.Storage.Storage. Each read
  -- once, after an action that settles through the probe.
  let routed hashed = mountingEach server $ do
        base <- Router.pageBase
        router <- if hashed then Router.newHashRouter Front spots else Router.newRouter base Front spots
        pure (routing, router)
      pathname page = evaluate page "location.pathname" :: IO Text
      -- Back and Forward, through the page's history: the promise resolves
      -- once the page has dispatched the popstate, after the client's
      -- listener and the router's (added before the test's), so the
      -- location and the event have been sent, and the settle after it asks
      -- the probe behind them on the same socket. Playwright's goBack waits
      -- for a load, which a traversal within the document does not give.
      traverse' page how = do
        True <- evaluate page ("new Promise((done) => { addEventListener('popstate', () => done(true), { once: true }); history." <> how <> "(); })")
        settle page
      back page = traverse' page "back"
      forward page = traverse' page "forward"
      under address = Text.dropWhile (/= '/') (Text.drop (Text.length "http://") address)

  it "opens a routed component at a deep route, and follows its links with the page's address, back and forward" $ inPage $ \page -> do
    (address, next) <- routed False
    visit page (address <> "blog/post/1")
    _ <- next
    readOnce page "#spot" `shouldReturn` "Post 1"
    -- What a page load would lose.
    True <- evaluate page "(window.stayed = true)"
    click (locator page "#to-about")
    readOnce page "#spot" `shouldReturn` "About"
    pathname page `shouldReturn` (under address <> "about")
    back page
    readOnce page "#spot" `shouldReturn` "Post 1"
    pathname page `shouldReturn` (under address <> "blog/post/1")
    forward page
    readOnce page "#spot" `shouldReturn` "About"
    pathname page `shouldReturn` (under address <> "about")
    evaluate page "window.stayed === true" `shouldReturn` True
    evaluate page "history.length" `shouldReturn` (3 :: Int)

  it "replaces the history's entry on a redirect, which Back skips" $ inPage $ \page -> do
    (address, next) <- routed False
    visit page (address <> "blog/post/1")
    _ <- next
    click (locator page "#to-front")
    readOnce page "#spot" `shouldReturn` "Front"
    pathname page `shouldReturn` under address
    click (locator page "#redirect")
    readOnce page "#spot" `shouldReturn` "About"
    pathname page `shouldReturn` (under address <> "about")
    back page
    readOnce page "#spot" `shouldReturn` "Post 1"
    pathname page `shouldReturn` (under address <> "blog/post/1")

  it "shows the route it is at again when the page is reloaded, in a new session" $ inPage $ \page -> do
    (address, next) <- routed False
    visit page address
    first' <- next
    click (locator page "#to-post")
    readOnce page "#spot" `shouldReturn` "Post 1"
    reload page
    waitStarted page
    second' <- next
    second'.session `shouldNotBe` first'.session
    readOnce page "#spot" `shouldReturn` "Post 1"
    pathname page `shouldReturn` (under address <> "blog/post/1")

  it "keeps a hash router's route in the page's fragment, back and forward" $ inPage $ \page -> do
    (address, next) <- routed True
    visit page (address <> "#/blog/post/1")
    _ <- next
    readOnce page "#spot" `shouldReturn` "Post 1"
    click (locator page "#to-about")
    readOnce page "#spot" `shouldReturn` "About"
    evaluate page "location.pathname + location.hash" `shouldReturn` (under address <> "#/about")
    back page
    readOnce page "#spot" `shouldReturn` "Post 1"
    evaluate page "location.hash" `shouldReturn` ("#/blog/post/1" :: Text)

  it "keeps a count in the page's own localStorage, which the next session reads after a reload" $ inPage $ \page -> do
    (address, next) <- mountingEach server (pure (storing, ()))
    visit page address
    first' <- next
    readOnce page "#count" `shouldReturn` "0"
    click (locator page "#bump")
    click (locator page "#bump")
    readOnce page "#count" `shouldReturn` "2"
    -- 2, as JSON, in base64, under the library's prefix.
    evaluate page "localStorage.getItem('haskell-halogen:count')" `shouldReturn` ("Mg==" :: Text)
    reload page
    waitStarted page
    second' <- next
    second'.session `shouldNotBe` first'.session
    readOnce page "#count" `shouldReturn` "2"
    click (locator page "#forget-count")
    evaluate page "localStorage.getItem('haskell-halogen:count')" `shouldReturn` (Nothing :: Maybe Text)

  it "reads on its first render what the page's localStorage held before it connected" $ inPage $ \page -> do
    -- 7, as JSON, in base64: put there by the page before any of its
    -- scripts run.
    addInitScript (pageContext page) "localStorage.setItem('haskell-halogen:count', 'Nw==')"
    (address, next) <- mountingEach server (pure (storing, ()))
    visit page address
    _ <- next
    readOnce page "#count" `shouldReturn` "7"

  it "keeps a count in the page's sessionStorage across a reload" $ inPage $ \page -> do
    (address, next) <- mountingEach server (pure (storing, ()))
    visit page address
    _ <- next
    click (locator page "#visit")
    readOnce page "#visits" `shouldReturn` "1"
    evaluate page "sessionStorage.getItem('haskell-halogen:visits')" `shouldReturn` ("MQ==" :: Text)
    reload page
    waitStarted page
    _ <- next
    readOnce page "#visits" `shouldReturn` "1"

  it "shows what a query from the test changed, once settled" $ inPage $ \page -> do
    (address, connected) <- mounting server view ()
    visit page address
    mounted <- connected
    queryComponent mounted (H.mkTell (SetLabel "from the test")) `shouldReturn` Just ()
    settle page
    readOnce page "#label" `shouldReturn` "from the test"

----------------------------------------------------------------------
-- A TCP proxy whose connections the test can cut.

data Proxy = Proxy
  { port :: Int
  , connections :: IORef [Socket.Socket]
  }

-- | Forward a free port to the server's, for the body.
withProxy :: Int -> (Proxy -> IO a) -> IO a
withProxy upstream body = do
  let hints = Socket.defaultHints {Socket.addrFlags = [Socket.AI_PASSIVE], Socket.addrSocketType = Socket.Stream}
  address : _ <- Socket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  bracket (Socket.openSocket address) Socket.close $ \listener -> do
    Socket.setSocketOption listener Socket.ReuseAddr 1
    Socket.bind listener (Socket.addrAddress address)
    Socket.listen listener 64
    port <- fromIntegral <$> Socket.socketPort listener
    connections <- newIORef []
    let proxy = Proxy {port, connections}
    bracket (forkIO (accepting proxy listener)) killThread (const (body proxy))
  where
    accepting proxy listener = forever $ do
      (client, _) <- Socket.accept listener
      void $ forkIO $ do
        let hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
        address : _ <- Socket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show upstream))
        server <- Socket.openSocket address
        Socket.connect server (Socket.addrAddress address)
        atomicModifyIORef' proxy.connections (\cs -> (client : server : cs, ()))
        let closeBoth = mapM_ (\s -> try @IOError (Socket.close s)) [client, server]
        done <- newEmptyTMVarIO
        -- A connection ending, from either side or cut, ends both
        -- directions quietly: errors here are how it ends.
        _ <- forkIO (void (try @IOError (pipe client server)) `finally` atomically (void (tryPutTMVar done ())))
        _ <- forkIO (void (try @IOError (pipe server client)) `finally` atomically (void (tryPutTMVar done ())))
        atomically (takeTMVar done) `finally` closeBoth
    pipe from to = do
      chunk <- Socket.recv from 65536
      if ByteString.null chunk
        then void (try @IOError (Socket.shutdown to Socket.ShutdownSend))
        else Socket.sendAll to chunk >> pipe from to

-- | Cut every connection the proxy carries, as a network that drops them:
-- both ends see them closed.
cut :: Proxy -> IO ()
cut proxy = do
  sockets <- atomicModifyIORef' proxy.connections ([],)
  mapM_ (\s -> try @IOError (Socket.shutdown s Socket.ShutdownBoth) >>= either (const (pure ())) pure) sockets
