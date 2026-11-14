{-# LANGUAGE TemplateHaskell #-}

-- | A component served over WebSockets, as a LiveView: each connection is a
-- session ("Halogen.Remote.Session") with an instance of the component of
-- its own, whose operations go to the page as text messages and whose
-- events come back as text messages.
--
-- The server also serves the client, at the WebSocket's path with @.js@
-- after it (@\/live.js@ for @\/live@), and whatever pages it is given, so a
-- page needs nothing but this program:
--
-- > <div id="app"></div>
-- > <script src="/live.js"></script>
-- > <script>halogen_remote_connect("/live", document.getElementById("app"));</script>
--
-- The client connects, shows what the session sends, and reconnects with a
-- growing delay when the connection is lost; each connection is a new
-- session, and a new instance of the component, so state that should
-- outlive a connection (or be shared between them, as a chat's lines are)
-- belongs outside the component, in something its program reads.
--
-- A component's program that throws ends its own session (the connection
-- is closed), not the server; a connection that closes disposes of its
-- session, and so finalizes its component.
--
-- 'serve' and 'application' mount one component, with one input, for every
-- connection. 'serveWith' and 'applicationWith' call a function of the
-- program's for each connection instead, with what the connection asked
-- for (its path and query string, its headers) and the new session, which
-- mounts what it wants ('Session.mount') and may keep the session and the
-- socket: a test that mounts a component of its own per page
-- ("Halogen.Remote.Test"), or a program that picks the component by the
-- address.
--
-- The server also answers the questions of the page's probe (the client
-- forwards them; see "Halogen.Remote.Protocol"): a page served this way can
-- be driven by @haskell-halogen-playwright@ as a page with the program in it
-- can.
--
-- __The page's address and stores.__ Before it calls anything for a
-- connection, the server waits for the client's first message, which says
-- where the page is (its address and its @\<base href>@) and what its
-- stores hold of this library's items, and hands it to the session. So a
-- router made in the session's monad ("Halogen.Router"'s @newRouter@, after
-- @pageBase@) starts at the page's route, and storage
-- ("Web.Storage.Storage") reads and writes the page's own @localStorage@
-- and @sessionStorage@: what a component stores outlives the connection, a
-- reload and the session. The client this server serves sends that message
-- as soon as it connects; a client that sends something else first gets a
-- session at @\/@ with empty stores, and one that sends nothing (a client
-- older than this message) is waited for.
module Halogen.Remote.Server
  ( Settings (..)
  , defaultSettings
  , serve
  , application
  , Request (..)
  , serveWith
  , applicationWith
  , clientScript
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (lookup)
import Data.Text qualified as Text
import Halogen.Component (Component)
import Halogen.Remote.DOM (RemoteDOM)
import Halogen.Remote.Session (Session)
import Halogen.Remote.Session qualified as Session
import Language.Haskell.TH.Syntax qualified as TH
import Network.HTTP.Types (RequestHeaders, hContentType, parseQueryText, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Protolude
import UnliftIO (tryAny)

-- | What a WebSocket connection asked for, as its opening request said.
data Request = Request
  { path :: Text
  -- ^ The path, without the query string: the settings' @path@.
  , query :: [(Text, Maybe Text)]
  -- ^ The query string, decoded: @?key=3&debug@ is
  -- @[(\"key\", Just \"3\"), (\"debug\", Nothing)]@.
  , headers :: RequestHeaders
  -- ^ The request's headers (a cookie, the origin).
  }
  deriving stock (Show)

data Settings = Settings
  { port :: Int
  -- ^ What 'serve' listens on.
  , path :: Text
  -- ^ Where the WebSocket is, and, with @.js@ after it, the client.
  , pages :: [(Text, FilePath, Text)]
  -- ^ Files to serve: the path a request asks for, the file, and its media
  -- type, as @(\"\/\", \"web\/live.html\", \"text\/html\")@. Each is read
  -- when it is asked for, so an edit shows on the next load.
  }

-- | Port 8081, the WebSocket at @\/live@, and no pages.
defaultSettings :: Settings
defaultSettings = Settings {port = 8081, path = "/live", pages = []}

-- | Serve the component until the program is stopped.
serve :: Settings -> Component q i o RemoteDOM -> i -> IO ()
serve settings component input = Warp.run settings.port (application settings component input)

-- | The server as a WAI application, for a program that serves other
-- things too: the WebSocket at the settings' path, the client and the
-- pages, and 404 for anything else.
application :: Settings -> Component q i o RemoteDOM -> i -> Wai.Application
application settings component input = applicationWith settings (\_ session -> void (Session.mount session component input))

-- | Serve, with a function called for each connection, until the program
-- is stopped.
serveWith :: Settings -> (Request -> Session -> IO ()) -> IO ()
serveWith settings connect = Warp.run settings.port (applicationWith settings connect)

-- | 'application', with a function called for each connection with what it
-- asked for and its new session: it mounts a component in the session
-- ('Session.mount') before it returns, and may keep the session and the
-- socket the mount gives. Only then does the server start handing the
-- session the page's messages.
--
-- A function that throws ends that session only, as a component's failure
-- does: the connection is closed with a reason, and the server goes on. A
-- function that mounts nothing has the connection closed with the reason
-- \"nothing was mounted for this connection\" (a test whose page asked for
-- a component it does not know, say), and the session disposed of. In
-- every case the session is disposed of when the connection ends, which
-- finalizes its component; and a session the program disposes of itself
-- (a test done with it) has its connection closed, with the reason \"the
-- session was disposed of\".
applicationWith :: Settings -> (Request -> Session -> IO ()) -> Wai.Application
applicationWith settings connect = websocketsOr WS.defaultConnectionOptions socket files
  where
    socket pending
      | requested.path == settings.path = do
          connection <- WS.acceptRequest pending
          WS.withPingThread connection 30 pass (connected connection)
      | otherwise = WS.rejectRequest pending "no WebSocket here"
      where
        request = WS.pendingRequest pending
        (bare, queryString) = ByteString.break (== 63) (WS.requestPath request)
        requested =
          Request
            { path = decodeUtf8 bare
            , query = parseQueryText queryString
            , headers = WS.requestHeaders request
            }

        connected connection = do
          session <- Session.newSession (WS.sendTextData connection)
          let program = do
                -- The page's first message says where it is and what its
                -- stores hold, which the function and the component it
                -- mounts read as they start. A client that says something
                -- else first has it handled as any message: the session
                -- keeps its defaults.
                WS.receiveData connection >>= Session.receive session
                connect requested session
                mounted <- Session.isMounted session
                if mounted
                  then -- A session disposed of by someone else (a test that
                  -- is done with it) closes its connection: the client
                  -- answers the close, which ends the loop.
                    withAsync (Session.ended session >> WS.sendClose connection ("the session was disposed of" :: Text)) $ \_ ->
                      forever (WS.receiveData connection >>= Session.receive session)
                  else do
                    WS.sendClose connection ("nothing was mounted for this connection" :: Text)
                    -- Read until the client's close answers ours.
                    forever (void (WS.receiveDataMessage connection))
          -- The connection closing ends the loop with an exception, which is
          -- how it always ends; anything else is the component's, and ends
          -- this session only.
          outcome <- tryAny program `finally` Session.dispose session
          case outcome of
            Left e
              | Just (_ :: WS.ConnectionException) <- fromException e -> pass
              | otherwise -> do
                  hPutStrLn stderr ("Halogen.Remote.Server: a session ended: " <> show e :: Text)
                  void (tryAny (WS.sendClose connection ("the session ended" :: Text)))
            Right () -> pass

    files request respond = case lookup requested served of
      Just (Left bytes) -> respond (Wai.responseLBS status200 [(hContentType, "text/javascript; charset=utf-8")] (LazyByteString.fromStrict bytes))
      Just (Right (file, media)) -> do
        bytes <- ByteString.readFile file
        respond (Wai.responseLBS status200 [(hContentType, encodeUtf8 media)] (LazyByteString.fromStrict bytes))
      Nothing -> respond (Wai.responseLBS status404 [(hContentType, "text/plain")] "not found")
      where
        requested = "/" <> Text.intercalate "/" (Wai.pathInfo request)
        served =
          (settings.path <> ".js", Left clientScript)
            : [(at, Right (file, media)) | (at, file, media) <- settings.pages]

-- | The client, @jsbits/remote.js@, as the library was built with it: what
-- a page loads to show a session, and what the loopback runs in a page.
clientScript :: ByteString
clientScript =
  encodeUtf8 $
    Text.pack
      $( do
           file <- TH.makeRelativeToProject "jsbits/remote.js"
           TH.addDependentFile file
           source <- TH.runIO (readFile file)
           TH.lift (Text.unpack source)
       )
