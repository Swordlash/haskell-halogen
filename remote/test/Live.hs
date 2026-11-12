-- | The whole of a LiveView at once: "Halogen.Remote.Server" natively,
-- Chromium driven by "Halogen.Playwright", the real @remote.js@ and a real
-- WebSocket. Each test reads both sides: the page through the driver, and
-- the component's side through what the test shares with it in this
-- process (a 'TVar' its handlers write) and "Halogen.Activity".
--
-- The browser reaches the server through a small TCP proxy the test can
-- cut, which is how a connection is lost from the server's side.
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM hiding (check)
import Control.Concurrent.STM qualified as STM
import Control.Exception (bracket, finally, try)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get, modify)
import Data.ByteString qualified as ByteString
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Halogen qualified as H
import Halogen.Activity qualified as Activity
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Remote.DOM (RemoteDOM)
import Halogen.Remote.Server qualified as Server
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket
import Network.Wai.Handler.Warp qualified as Warp
import Prelude
import System.Timeout (timeout)

-- | What the sessions' components tell the test.
data Shared = Shared
  { nextSession :: TVar Int
  , counts :: TVar (Map Int Int)
  -- ^ Each live session's count.
  , typed :: TVar (Map Int Text)
  , finalized :: TVar [Int]
  }

newShared :: IO Shared
newShared = Shared <$> newTVarIO 1 <*> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO []

data State = State {session :: Int, count :: Int, typedText :: Text}

data Action = Increment | Typed Text

-- | A counter and an input, each session numbered: what its handlers do
-- they also write where the test can see it.
live :: Shared -> H.Component H.VoidF () Void RemoteM
live shared =
  H.component
    (\() -> liftIO (atomically (stateTVar shared.nextSession (\n -> (n, n + 1)))) >>= \n -> pure State {session = n, count = 0, typedText = ""})
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
    withProxy serverPort $ \proxy -> hspec (spec shared proxy)

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
