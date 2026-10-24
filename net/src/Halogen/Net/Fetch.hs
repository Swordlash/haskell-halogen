{-# LANGUAGE TemplateHaskell #-}

-- | The browser's @fetch@: a request from the page, and its response.
--
-- A 'Request' is a value: 'request' makes one with the browser's defaults,
-- and record update changes what else it should say.
--
-- > save :: (MonadIO m) => Text -> m (Either JSError Response)
-- > save json =
-- >   fetch
-- >     (request POST "/api/notes")
-- >       { headers = [("content-type", "application/json")]
-- >       , body = Just (TextBody json)
-- >       }
--
-- 'fetch' starts the request and waits for its response, that is for its
-- status and headers; the body is read afterwards, with 'text' or 'bytes',
-- which wait for the rest. Waiting is why these belong in a component's
-- @lift@ (or @liftIO@), which waits on a worker thread while the rest of the
-- tree goes on, and never in @H.liftEffect@, which must not wait.
--
-- Some requests have to start at once, inside the dispatch of the event that
-- asked for them, or are to be kept so that another action can abort them.
-- 'start' waits for nothing, so it can run in @H.liftEffect@: it gives a
-- 'Fetch', a handle for the component to keep in its state, and 'response'
-- waits for it later, in @lift@, as 'fetch' would have.
--
-- = What fails
--
-- What the browser rejects is a 'Left', never an exception: a request that
-- cannot be made or did not get a response (an address that is no address,
-- a host that does not answer, a request the page's CORS rules refuse, a
-- header name @fetch@ will not send, a body on a @GET@) is a @TypeError@,
-- and an aborted one an @AbortError@ (each a 'JSError' by its @name@, with
-- the browser's own @message@). A response with an error status is still a
-- response, as it is in the browser: a 404 is a @'Right'@ whose @ok@ is
-- 'False' and whose @status@ is 404, with a body to read like any other.
--
-- = Cancellation
--
-- A 'fetch' owns its request, so a wait that ends early aborts it: when the
-- thread waiting in 'fetch' is killed, as a component's worker is when the
-- component goes away or its fiber is killed, the request is aborted rather
-- than left to run to the end unread. So is the request of a response
-- 'fetch' gave, when a wait for its body in 'text' or 'bytes' is killed.
-- The abort happens as the kill is delivered, and a thread blocked in the
-- wait takes a kill at once on both browser backends (WebAssembly and
-- JavaScript), without waiting for the response first; the browser then
-- drops the connection, or the request if it has not gone yet.
--
-- A 'Fetch' from 'start' is the program's, and only 'abort' aborts it: a
-- killed wait in 'response', or in 'text' or 'bytes' on what it gave, only
-- stops waiting, since another wait may still want it. A component that keeps
-- one aborts it itself when it should no longer run, in its @Finalize@ say.
-- Once aborted, a wait for its response, or for the body if the response has
-- arrived, gives @'JSError' "AbortError" _@.
--
-- = Natively
--
-- There is no engine to make a request natively, and this is no HTTP client
-- of its own: 'fetch' and 'response' give @'JSError' "NotInBrowserError" _@
-- at once, as 'Halogen.JSBits.Promise.await' does, and 'abort' does nothing.
module Halogen.Net.Fetch
  ( -- * Requests
    Request (..)
  , request
  , Method (..)
  , Body (..)
  , Credentials (..)
  , Mode (..)

    -- * Fetching
  , fetch

    -- * Starting now, waiting later
  , Fetch
  , start
  , response
  , abort

    -- * Responses
  , Response
  , text
  , bytes

    -- * Failures
  , JSError (..)
  )
where

import Control.Exception (bracketOnError)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Records (HasField (..))
import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, fromJSBytes, fromJSText, jsValText, toJSBytes, toJSText)
import Halogen.JSBits.Promise (JSError (..), Promise (..), await)

-- | What to ask for: the arguments of the browser's @fetch@.
data Request = Request
  { method :: Method
  , url :: Text
  -- ^ Absolute, or relative to the page's address.
  , headers :: [(Text, Text)]
  -- ^ Sent in this order. A header the browser keeps for itself (@host@,
  -- @cookie@, @content-length@, …) is left out without a word, as @fetch@
  -- leaves it out.
  , body :: Maybe Body
  -- ^ A @GET@ or @HEAD@ request with one is refused, with a @TypeError@.
  , credentials :: Credentials
  , mode :: Mode
  }
  deriving stock (Eq, Show)

-- | A request with the browser's defaults: no headers of its own, no body,
-- 'SameOriginCredentials' and 'CorsMode'.
request :: Method -> Text -> Request
request method url =
  Request
    { method
    , url
    , headers = []
    , body = Nothing
    , credentials = SameOriginCredentials
    , mode = CorsMode
    }

-- | The request's method. 'OtherMethod' is sent as it is spelt, and is
-- refused with a @TypeError@ if it is no HTTP token or is one the browser
-- does not send (@CONNECT@, @TRACE@, @TRACK@).
data Method
  = GET
  | HEAD
  | POST
  | PUT
  | PATCH
  | DELETE
  | OPTIONS
  | OtherMethod Text
  deriving stock (Eq, Show)

-- | What a request sends, encoded already: this package encodes nothing
-- itself, JSON included. Text goes as UTF-8, and the browser gives it a
-- @content-type@ of @text/plain;charset=UTF-8@ unless the request names one;
-- bytes go as they are, with no @content-type@ unless the request names one.
data Body
  = TextBody Text
  | BytesBody ByteString
  deriving stock (Eq, Show)

-- | Whether the request carries the browser's cookies and other credentials,
-- and takes those of the response: never, only to the page's own origin (the
-- browser's default), or to any origin (which then has to allow it).
data Credentials
  = OmitCredentials
  | SameOriginCredentials
  | IncludeCredentials
  deriving stock (Eq, Show)

-- | Which requests to other origins the browser makes: 'CorsMode' (the
-- browser's default) makes them under its CORS rules; 'NoCorsMode' makes
-- them without, and gives an opaque response (status 0, no headers, an empty
-- body) where the rules would have been needed; 'SameOriginMode' makes none,
-- and refuses them with a @TypeError@.
data Mode
  = CorsMode
  | NoCorsMode
  | SameOriginMode
  deriving stock (Eq, Show)

-- | A request under way, from 'start': the request's handle, which holds the
-- @AbortController@ it was started with, and the promise of its response.
data Fetch = Fetch
  { handle :: JSVal
  , promise :: Promise
  }

-- | A response, once its status and headers have arrived, read through its
-- fields:
--
-- [@status@ :: 'Int']: the HTTP status, or 0 for an opaque response.
-- [@statusText@ :: 'Text']: the status line's text (empty over HTTP/2).
-- [@ok@ :: 'Bool']: whether the status is in 200–299.
-- [@url@ :: 'Text']: the address the response came from, after any
--   redirects.
-- [@headers@ :: @[('Text', 'Text')]@]: the headers the page may read, as the
--   browser's @Headers@ lists them: names in lower case and sorted, and a
--   header sent more than once as one, its values joined with @", "@.
--
-- Its body is still on its way: read it with 'text' or 'bytes', once. The
-- browser keeps no copy of a body it has handed over, so a second read, of
-- either kind, gives a @TypeError@.
data Response = Response
  { responseStatus :: Int
  , responseStatusText :: Text
  , responseOk :: Bool
  , responseUrl :: Text
  , responseHeaders :: [(Text, Text)]
  , raw :: JSVal
  -- ^ The browser's @Response@.
  , owner :: Maybe Fetch
  -- ^ The request to abort when a wait for the body is killed: that of a
  -- 'fetch', which owns it, and none for 'response'.
  }

-- The fields are read through these rather than declared under their own
-- names because 'Request' has fields named @url@ and @headers@ too: with both
-- in scope a record update of a 'Request''s @headers@ would be ambiguous, and
-- refused.
instance HasField "status" Response Int where getField r = r.responseStatus

instance HasField "statusText" Response Text where getField r = r.responseStatusText

instance HasField "ok" Response Bool where getField r = r.responseOk

instance HasField "url" Response Text where getField r = r.responseUrl

instance HasField "headers" Response [(Text, Text)] where getField r = r.responseHeaders

$( browserJS
     ["jsbits/fetch.js"]
     [ ("js_request", "halogen_net_fetch_request", Unsafe, [t|JSText -> JSText -> JSText -> JSText -> IO JSVal|])
     , ("js_header", "halogen_net_fetch_header", Unsafe, [t|JSVal -> JSText -> JSText -> IO ()|])
     , ("js_text_body", "halogen_net_fetch_body", Unsafe, [t|JSVal -> JSText -> IO ()|])
     , ("js_bytes_body", "halogen_net_fetch_body", Unsafe, [t|JSVal -> JSVal -> IO ()|])
     , ("js_start", "halogen_net_fetch_start", Unsafe, [t|JSVal -> IO Promise|])
     , ("js_abort", "halogen_net_fetch_abort", Unsafe, [t|JSVal -> IO ()|])
     , ("js_status", "halogen_net_response_status", Unsafe, [t|JSVal -> IO Int|])
     , ("js_status_text", "halogen_net_response_status_text", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_ok", "halogen_net_response_ok", Unsafe, [t|JSVal -> IO Bool|])
     , ("js_url", "halogen_net_response_url", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_headers", "halogen_net_response_headers", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_text", "halogen_net_response_text", Unsafe, [t|JSVal -> IO Promise|])
     , ("js_bytes", "halogen_net_response_bytes", Unsafe, [t|JSVal -> IO Promise|])
     ]
 )

-- | Make the request and wait for its response: its status and headers, not
-- its body. Call it from @lift@. Killing the thread that waits aborts the
-- request (see /Cancellation/ above).
fetch :: (MonadIO m) => Request -> m (Either JSError Response)
fetch r = liftIO $ bracketOnError (start r) abort (\started -> waitFor (Just started) started)

-- | Start the request, and wait for nothing: by the time this returns, the
-- browser has the request. It is synchronous, so it can run in
-- @H.liftEffect@, inside the dispatch of the event that asked for it; wait
-- for the response with 'response'.
start :: (MonadIO m) => Request -> m Fetch
start r = liftIO $ do
  handle <- js_request (toJSText (methodName r.method)) (toJSText r.url) (toJSText (credentialsName r.credentials)) (toJSText (modeName r.mode))
  for_ r.headers $ \(name, value) -> js_header handle (toJSText name) (toJSText value)
  for_ r.body $ \case
    TextBody t -> js_text_body handle (toJSText t)
    BytesBody b -> js_bytes_body handle =<< toJSBytes b
  promise <- js_start handle
  pure Fetch {handle, promise}

-- | Wait for the response of a request 'start' started, as 'fetch' does. Any
-- number of waits for the same 'Fetch' each get its response, but there is
-- one body between them. Killing the thread that waits stops the wait and
-- nothing else; 'abort' aborts the request.
response :: (MonadIO m) => Fetch -> m (Either JSError Response)
response started = liftIO (waitFor Nothing started)

-- | Abort the request: a wait for its response, or for its body if the
-- response has arrived, then gives @'JSError' "AbortError" _@. Once the body
-- has been read it does nothing, as it does natively.
abort :: (MonadIO m) => Fetch -> m ()
abort started = liftIO (js_abort started.handle)

-- | Read the body as text, decoded as UTF-8 (as the browser decodes it,
-- whatever the response's @content-type@ says), waiting for all of it.
-- A body can be read once: see 'Response'.
text :: (MonadIO m) => Response -> m (Either JSError Text)
text r = liftIO $ fmap jsValText <$> reading r (js_text r.raw)

-- | Read the body as bytes, waiting for all of it. A body can be read once:
-- see 'Response'.
bytes :: (MonadIO m) => Response -> m (Either JSError ByteString)
bytes r = liftIO $ traverse fromJSBytes =<< reading r (js_bytes r.raw)

waitFor :: Maybe Fetch -> Fetch -> IO (Either JSError Response)
waitFor owner started = traverse (responseOf owner) =<< await started.promise

-- | Wait for the body, aborting the request of a response that owns it if
-- the wait is killed. Masked until the wait has begun, so that no kill comes
-- between starting the read and being ready to abort.
reading :: Response -> IO Promise -> IO (Either JSError JSVal)
reading r begin = bracketOnError begin (const (traverse_ abort r.owner)) await

responseOf :: Maybe Fetch -> JSVal -> IO Response
responseOf owner raw = do
  responseStatus <- js_status raw
  responseStatusText <- fromJSText <$> js_status_text raw
  responseOk <- js_ok raw
  responseUrl <- fromJSText <$> js_url raw
  responseHeaders <- headerLines . fromJSText <$> js_headers raw
  pure Response {responseStatus, responseStatusText, responseOk, responseUrl, responseHeaders, raw, owner}

-- | The lines of @name:value@ that jsbits/fetch.js writes: a name has no
-- colon in it, so each line is split at its first.
headerLines :: Text -> [(Text, Text)]
headerLines listed
  | Text.null listed = []
  | otherwise = map pair (Text.splitOn "\n" listed)
  where
    pair line = let (name, rest) = Text.breakOn ":" line in (name, Text.drop 1 rest)

methodName :: Method -> Text
methodName = \case
  GET -> "GET"
  HEAD -> "HEAD"
  POST -> "POST"
  PUT -> "PUT"
  PATCH -> "PATCH"
  DELETE -> "DELETE"
  OPTIONS -> "OPTIONS"
  OtherMethod name -> name

credentialsName :: Credentials -> Text
credentialsName = \case
  OmitCredentials -> "omit"
  SameOriginCredentials -> "same-origin"
  IncludeCredentials -> "include"

modeName :: Mode -> Text
modeName = \case
  CorsMode -> "cors"
  NoCorsMode -> "no-cors"
  SameOriginMode -> "same-origin"
