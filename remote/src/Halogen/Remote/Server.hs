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
module Halogen.Remote.Server
  ( Settings (..)
  , defaultSettings
  , serve
  , application
  , clientScript
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (lookup)
import Data.Text qualified as Text
import Halogen.Component (Component)
import Halogen.Remote.DOM (RemoteDOM)
import Halogen.Remote.Session qualified as Session
import Language.Haskell.TH.Syntax qualified as TH
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Protolude
import UnliftIO (tryAny)

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
application settings component input = websocketsOr WS.defaultConnectionOptions socket files
  where
    socket pending
      | WS.requestPath (WS.pendingRequest pending) == encodeUtf8 settings.path = do
          connection <- WS.acceptRequest pending
          WS.withPingThread connection 30 pass (connected connection)
      | otherwise = WS.rejectRequest pending "no WebSocket here"

    connected connection = do
      session <- Session.newSession (WS.sendTextData connection)
      let program = do
            _ <- Session.mount session component input
            forever (WS.receiveData connection >>= Session.receive session)
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
