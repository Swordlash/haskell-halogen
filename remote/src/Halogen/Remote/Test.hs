-- | What a test suite uses to test a component in a real page while the
-- component runs in the test's own process: a server
-- ("Halogen.Remote.Server") on a free port, and for each example a page of
-- its own, which shows a component mounted for that page alone, through
-- the client (@jsbits/remote.js@).
--
-- With @haskell-halogen-playwright@ driving the page (this module does not
-- depend on it; a test combines the two):
--
-- > main :: IO ()
-- > main = withTestServer $ \server -> hspec $ playwright $
-- >   it "saves" $ inPage $ \page -> do
-- >     (address, connected) <- mounting server (editor env) input
-- >     visit page address                          -- returns once the first render is shown
-- >     mounted <- connected
-- >     click (byRoleNamed page "button" "Save")    -- settles: handled, rendered and sent
-- >     textContent (locator page "#status") `shouldReturn` "Saved"
-- >     queryComponent mounted (H.mkRequest IsSaved) `shouldReturn` Just True
--
-- The page answers the driver's probe through the wire, for its own
-- session alone ("Halogen.Remote.Protocol"): an action settles once the
-- component has handled it and the page shows what it rendered, whatever
-- the other examples' components are doing, so examples can run in parallel
-- against one server.
--
-- Each 'mounting' registers the component under a key of its own, which is
-- in the address it gives; the page at that address connects once, without
-- reconnecting, to the WebSocket with the same key, and the server mounts
-- that component for it and no other. A key 'mounting' registers is
-- mounted once: a second connection with it (the page reloaded) gets
-- nothing, and its connection is closed with a reason. When the page
-- closes its connection the session is disposed of (the component
-- finalized), and when 'withTestServer' returns every session is.
--
-- = Routes and reloads
--
-- The address a key is at, @\/mount\/\<key>\/@, is the page's base: the
-- server answers every address under it with the same page, whose
-- @\<base href>@ names it, so a page can be opened at a route
-- (@address <> \"blog\/post\/1\"@) and "Halogen.Router"'s @pageBase@ is
-- that base. The client says first where its page is and what its stores
-- hold ("Halogen.Remote.Protocol"), before the component is mounted, so a
-- router made in the session's monad starts at the page's route, and
-- storage is the page's own.
--
-- 'mountingEach' registers a program, run in each new session's monad
-- after the page has said where it is, that makes the component and its
-- input: a router is made there, as an application's @main@ makes it.
-- Every connection with that key (the page reloaded, a second page opened
-- at the address) mounts what the program makes then, a new session, and
-- the action it gives waits for the next of them, in order:
--
-- > (address, next) <- mountingEach server $ do
-- >   base <- Router.pageBase
-- >   router <- Router.newRouter base Home route
-- >   pure (app, router)
-- > visit page (address <> "blog/post/1")
-- > first <- next
-- > reload page >> waitStarted page
-- > second <- next                               -- the reloaded page's
module Halogen.Remote.Test
  ( -- * The server
    TestServer
  , withTestServer
  , withTestServerWith
  , TestSettings (..)
  , defaultTestSettings
  , testServerAddress

    -- * A component per page
  , mounting
  , mountingEach
  , Mounted (..)
  , queryComponent
  , TestServerError (..)
  )
where

import Control.Concurrent.STM qualified as STM
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Halogen.Component (Component)
import Halogen.IO.Driver (HalogenSocket (..))
import Halogen.Remote.DOM (RemoteDOM)
import Halogen.Remote.Server qualified as Server
import Halogen.Remote.Session (Session)
import Halogen.Remote.Session qualified as Session
import Network.HTTP.Types (hContentType, status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Protolude
import System.Timeout (timeout)
import UnliftIO (tryAny)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef)

-- | How the test server's pages are made.
data TestSettings = TestSettings
  { shell :: Text -> Text
  -- ^ The page at a 'mounting' address, given the HTML that shows the
  -- component (its container and the client's scripts), which goes in the
  -- body: where a suite adds the application's stylesheets or scripts of
  -- its own. The default is a bare HTML document. The server puts a
  -- @\<base href>@ naming the key's address first in its @\<head>@ (first
  -- in the page, for a shell with none), so a relative address in it is
  -- read against the key's address, whatever route the page is at; a
  -- @\<base>@ of the shell's own comes after it, and does nothing.
  , pages :: [(Text, FilePath, Text)]
  -- ^ Other files to serve, as "Halogen.Remote.Server"'s 'Server.pages':
  -- the path, the file and its media type (the application's CSS, say).
  , connectTimeout :: Int
  -- ^ How long the action 'mounting' gives waits for its page to connect,
  -- in microseconds (ten seconds).
  }

defaultTestSettings :: TestSettings
defaultTestSettings =
  TestSettings
    { shell = \body ->
        Text.unlines
          [ "<!doctype html>"
          , "<html lang=\"en\">"
          , "<head><meta charset=\"utf-8\"><title>Halogen.Remote.Test</title></head>"
          , "<body>"
          , body
          , "</body>"
          , "</html>"
          ]
    , pages = []
    , connectTimeout = 10_000_000
    }

-- | A server for a test's components.
data TestServer = TestServer
  { address :: Text
  , settings :: TestSettings
  , keys :: IORef Int
  , waiting :: STM.TVar (Map Text Registered)
  -- ^ What each key's connection mounts: for a key of 'mounting''s, until
  -- one has.
  , sessions :: STM.TVar [Session]
  -- ^ Every session the server has mounted something in, to dispose of at
  -- the end.
  }

-- | What a key's connections mount, and whether only the first does.
data Registered = Registered
  { once :: Bool
  , mountIt :: Session -> IO ()
  }

-- | The server's address, @http:\/\/127.0.0.1:<port>@, for a page of its
-- 'pages'.
testServerAddress :: TestServer -> Text
testServerAddress server = server.address

data TestServerError
  = -- | No page connected to the address within the settings'
    -- 'connectTimeout'.
    NoConnection Text
  deriving stock (Show)

instance Exception TestServerError

-- | A component a page shows, mounted for it in the test's process.
data Mounted q o = Mounted
  { session :: Session
  , socket :: HalogenSocket q o RemoteDOM
  }

-- | A query of the mounted component, through its socket: what
-- @'Session.run' session (socket.query q)@ would be, if a rank-2 field could
-- be read with a dot.
queryComponent :: Mounted q o -> q a -> IO (Maybe a)
queryComponent mounted q = case mounted.socket of
  HalogenSocket {query = send} -> Session.run mounted.session (send q)

-- | A server on a free port of the loopback interface, for the duration of
-- the action, with 'defaultTestSettings'.
withTestServer :: (TestServer -> IO a) -> IO a
withTestServer = withTestServerWith defaultTestSettings

withTestServerWith :: TestSettings -> (TestServer -> IO a) -> IO a
withTestServerWith settings action = do
  keys <- newIORef 1
  waiting <- STM.newTVarIO Map.empty
  sessions <- STM.newTVarIO []
  let socketPath = "/halogen-remote-test"
      serverSettings = Server.defaultSettings {Server.path = socketPath, Server.pages = settings.pages}
      connect request session = do
        let key = join (lookup "mount" request.query)
        registered <- STM.atomically $ do
          found <- maybe (pure Nothing) (\k -> Map.lookup k <$> STM.readTVar waiting) key
          for_ found $ \r -> do
            when r.once $ for_ key (STM.modifyTVar' waiting . Map.delete)
            STM.modifyTVar' sessions (session :)
          pure found
        for_ registered (\r -> r.mountIt session)
      live = Server.applicationWith serverSettings connect
      app request respond = case Wai.pathInfo request of
        "mount" : key : _ -> respond (Wai.responseLBS status200 [(hContentType, "text/html; charset=utf-8")] (page key))
        _ -> live request respond
      page key = LazyByteString.fromStrict (encodeUtf8 (based key (settings.shell (body key))))
      -- The page's base, first in its head (before anything in it that a
      -- relative address could name), or first in the body for a shell
      -- with no head of its own.
      based key html = case Text.breakOn "<head>" html of
        (before, after) | not (Text.null after) -> before <> "<head>" <> baseTag key <> Text.drop 6 after
        _ -> baseTag key <> html
      baseTag key = "<base href=\"/mount/" <> key <> "/\">"
      body key =
        Text.unlines
          [ "<div id=\"halogen-remote-test\"></div>"
          , "<script src=\"" <> socketPath <> ".js\"></script>"
          , "<script>halogen_remote_connect(\""
              <> socketPath
              <> "?mount="
              <> key
              <> "\", document.getElementById(\"halogen-remote-test\"), { reconnect: false });</script>"
          ]
  Warp.testWithApplication (pure app) $ \port -> do
    let server = TestServer {address = "http://127.0.0.1:" <> show port, settings, keys, waiting, sessions}
    action server `finally` (STM.readTVarIO sessions >>= traverse_ Session.dispose)

-- | Register a component for one page: the address of the page that shows
-- it, and an action that waits for that page to connect (the settings'
-- 'connectTimeout' at most) and gives the component mounted for it. A
-- component that fails to mount has the action throw what it threw.
--
-- The address ends with a @\/@: it is the page's base, and an address
-- under it opens the same page at a route ("Routes and reloads", above).
mounting :: TestServer -> Component q i o RemoteDOM -> i -> IO (Text, IO (Mounted q o))
mounting server component input = do
  key <- newKey server
  box <- newEmptyMVar
  let mountIt session = do
        outcome <- tryAny (Session.mount session component input)
        _ <- tryPutMVar box (Mounted session <$> outcome)
        either throwIO (const pass) outcome
  STM.atomically $ STM.modifyTVar' server.waiting (Map.insert key Registered {once = True, mountIt})
  let address = addressOf server key
      connected =
        timeout server.settings.connectTimeout (readMVar box) >>= \case
          Nothing -> throwIO (NoConnection address)
          Just outcome -> either throwIO pure outcome
  pure (address, connected)

-- | Register a program for a page that may connect more than once (be
-- reloaded, or opened again): the address of the page, and an action that
-- waits for the next connection with it (the settings' 'connectTimeout' at
-- most) and gives what was mounted for it. Each connection runs the
-- program in its new session's monad, once the page has said where it is
-- and what its stores hold, and mounts the component it makes with the
-- input it makes (a router made there reads the page's address). The
-- action gives the connections in the order they came, each once: the
-- first time it is run, the first connection's, and so on. A program or a
-- component that fails ends that connection's session, and the action
-- gives it throws what it threw.
mountingEach :: TestServer -> RemoteDOM (Component q i o RemoteDOM, i) -> IO (Text, IO (Mounted q o))
mountingEach server program = do
  key <- newKey server
  arrived <- STM.newTQueueIO
  let mountIt session = do
        outcome <- tryAny $ do
          (component, input) <- Session.run session program
          Mounted session <$> Session.mount session component input
        STM.atomically (STM.writeTQueue arrived outcome)
        either throwIO (const pass) outcome
  STM.atomically $ STM.modifyTVar' server.waiting (Map.insert key Registered {once = False, mountIt})
  let address = addressOf server key
      next =
        timeout server.settings.connectTimeout (STM.atomically (STM.readTQueue arrived)) >>= \case
          Nothing -> throwIO (NoConnection address)
          Just outcome -> either throwIO pure outcome
  pure (address, next)

newKey :: TestServer -> IO Text
newKey server = show <$> atomicModifyIORef' server.keys (\n -> (n + 1, n :: Int))

-- | A key's page, which is its base too.
addressOf :: TestServer -> Text -> Text
addressOf server key = server.address <> "/mount/" <> key <> "/"
