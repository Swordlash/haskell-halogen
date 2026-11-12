{-# LANGUAGE TemplateHaskell #-}

-- | The connection to the node process that drives Chromium: starting it,
-- the protocol, and ending it. "Halogen.Playwright" is the public face;
-- this module is exposed for tests of the package itself and for a
-- program that needs a call the API does not have yet.
--
-- The protocol is JSON lines over the child's stdin and stdout, described
-- at the top of @js/driver.mjs@: a call carries an @id@ and gets exactly one
-- reply with it, so several threads can call at once, their writes
-- serialised and their replies matched by id; anything node sends that is
-- not a reply is told apart by its keys: an @event@, or an @ask@ (a routed
-- request), which this side answers once with an @answer@.
--
-- The thread that reads node's output never waits for anything but that
-- output: it does not write (node writes its stdout synchronously, so a
-- reader that waited on a write could leave both processes waiting on each
-- other), and it runs no handler. Each ask is answered from a thread of its
-- own.
module Halogen.Playwright.Internal
  ( -- * The connection
    Playwright (..)
  , Options (..)
  , defaultOptions
  , withPlaywright
  , call
  , call_
  , PlaywrightError (..)
  , driverProcessId
  , killDriver

    -- * Contexts and pages
  , Context (..)
  , Page (..)

    -- * Routes
  , addRoute
  , removeRoute

    -- * What pages said, and what contexts asked for
  , ConsoleMessage (..)
  , Dialog (..)
  , PageLog (..)
  , pageLog
  , waitForLog
  , Request (..)
  , contextRequests
  , FailedRequest (..)
  , failedRequests
  , routeFailures

    -- * The driver's source
  , driverScript
  )
where

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, orElse, readTVar, readTVarIO, registerDelay)
import Control.Exception (Exception (..), IOException, SomeAsyncException, SomeException, bracket, evaluate, finally, mask_, onException, throwIO, try, uninterruptibleMask_)
import Control.Monad (forM_, forever, unless, void, when)
import Data.Aeson (FromJSON, Value (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (for_)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Language.Haskell.TH.Syntax qualified as TH
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (BufferMode (..), Handle, hClose, hFlush, hIsEOF, hPutStrLn, hSetBinaryMode, hSetBuffering, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (Signal, sigKILL, sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (getPid)
import System.Process.Typed qualified as Process

-- | How to start the browser.
data Options = Options
  { headless :: Bool
  , slowMotion :: Int
  -- ^ Milliseconds Playwright waits before every action, to follow a test
  -- with the eye.
  , timeout :: Int
  -- ^ Milliseconds an action, a wait or an expectation may take before it
  -- fails (Playwright's default timeout, and the waits' own).
  , node :: FilePath
  -- ^ The @node@ to run the driver with.
  , artifacts :: Maybe FilePath
  -- ^ Where "Halogen.Playwright.Hspec"'s 'Halogen.Playwright.Hspec.inPage'
  -- saves a failing test's screenshot.
  , relayConsole :: Bool
  -- ^ Whether what pages write to their console, and what they throw, is
  -- printed to stderr as it arrives (each line prefixed with its page), as
  -- well as kept.
  , throttle :: Maybe Double
  -- ^ A processor slowdown every page gets as it is made (Chromium's
  -- @Emulation.setCPUThrottlingRate@: 4 is four times as slow), so that a
  -- run of the tests finds the races a slow machine would.
  , browserArgs :: [Text]
  -- ^ Command-line switches for Chromium, such as
  -- @--host-resolver-rules=MAP wiki.example.test 127.0.0.1@.
  }
  deriving stock (Show)

-- | Headless, at full speed, five seconds, the @node@ on the path, no
-- screenshots, the console kept but not printed, no throttle, no switches.
defaultOptions :: Options
defaultOptions =
  Options
    { headless = True
    , slowMotion = 0
    , timeout = 5000
    , node = "node"
    , artifacts = Nothing
    , relayConsole = False
    , throttle = Nothing
    , browserArgs = []
    }

-- | A running driver: one node process and its Chromium.
data Playwright = Playwright
  { options :: Options
  , input :: Handle
  , process :: Process.Process Handle Handle Handle
  , writing :: MVar ()
  -- ^ Held while a message is written, so that lines do not interleave.
  , pending :: IORef (IntMap (MVar (Either PlaywrightError Value)))
  , nextId :: IORef Int
  , gone :: IORef (Maybe Text)
  -- ^ Why the driver can no longer be used, once it cannot.
  , logs :: IORef (IntMap PageLog)
  , logged :: TVar Int
  -- ^ Changed whenever a page's log is, for a wait on what a page says.
  , requestLogs :: IORef (IntMap (Seq Request))
  -- ^ Each context's requests, oldest first.
  , failures :: IORef (IntMap ContextFailures)
  -- ^ Each context's failed requests and failed route handlers.
  , routes :: IORef (IntMap RouteEntry)
  -- ^ The routes, by the id this side gave them, with their handlers'
  -- threads under way.
  , errorLines :: IORef (Seq Text)
  -- ^ The last lines node wrote to stderr, for the message when it dies.
  }

-- | A route's handler and the threads answering its asks now, by ask.
data RouteEntry = RouteEntry
  { context :: Int
  , pattern :: Text
  , handler :: Aeson.Object -> IO [Aeson.Pair]
  , inFlight :: IntMap ThreadId
  }

-- | What went wrong in a context that a failure should mention.
data ContextFailures = ContextFailures
  { requests :: Seq FailedRequest
  , handlers :: Seq Text
  }

-- | A browser context: pages that share cookies and storage, and nothing
-- with another context's.
data Context = Context
  { playwright :: Playwright
  , handle :: Int
  }

-- | A page (a tab) of a context.
data Page = Page
  { playwright :: Playwright
  , handle :: Int
  , context :: Int
  -- ^ Its context's handle.
  , callTimeout :: Maybe Int
  -- ^ Milliseconds that take the place of the options' 'timeout' for this
  -- handle's calls, and its locators' ('Halogen.Playwright.withTimeout').
  }

-- | A failed call: the method, Playwright's name for the error (a
-- @TimeoutError@'s message is Playwright's own, which names the locator
-- and what it waited for), and its message.
data PlaywrightError = PlaywrightError
  { method :: Text
  , name :: Text
  , message :: Text
  }
  deriving stock (Eq, Show)

instance Exception PlaywrightError where
  displayException e = Text.unpack ("Playwright " <> e.method <> ": " <> e.name <> ": " <> e.message)

-- | Something a page wrote to its console: its type (@log@, @error@,
-- @warning@, ...) and its text.
data ConsoleMessage = ConsoleMessage
  { kind :: Text
  , text :: Text
  }
  deriving stock (Eq, Show)

-- | A dialog a page opened (an @alert@, a @confirm@, a @beforeunload@
-- asking whether to leave): its type and its message. The driver answered
-- it at once, as the context's options say.
data Dialog = Dialog
  { kind :: Text
  , message :: Text
  }
  deriving stock (Eq, Show)

-- | What a page has said, oldest first.
data PageLog = PageLog
  { console :: Seq ConsoleMessage
  , errors :: Seq Text
  -- ^ Uncaught exceptions and unhandled rejections, with their stacks.
  , dialogs :: Seq Dialog
  }

emptyLog :: PageLog
emptyLog = PageLog Seq.empty Seq.empty Seq.empty

-- | What a page has said so far.
pageLog :: Playwright -> Int -> IO PageLog
pageLog pw page = fromMaybe emptyLog . IntMap.lookup page <$> readIORef pw.logs

-- | Wait until a reading of a page's log gives something, up to a number
-- of milliseconds; 'Nothing' if it has not by then. It reads the log
-- again each time the driver's reader adds to any page's, and never on a
-- clock.
waitForLog :: Playwright -> Int -> Int -> (PageLog -> Maybe a) -> IO (Maybe a)
waitForLog pw page ms reading = do
  late <- registerDelay (ms * 1000)
  let loop = do
        -- Read before the log, so that an entry added after the log was
        -- read changes what was seen, and is read on the next round.
        seen <- readTVarIO pw.logged
        found <- reading <$> pageLog pw page
        case found of
          Just a -> pure (Just a)
          Nothing -> do
            more <-
              atomically $
                ((readTVar pw.logged >>= check . (/= seen)) >> pure True)
                  `orElse` (readTVar late >>= check >> pure False)
            if more then loop else pure Nothing
  loop

-- | A request a page of a context, or a worker of one of its pages, made:
-- its address, its method and Playwright's name for what it is for
-- (@document@, @script@, @fetch@, ...).
data Request = Request
  { url :: Text
  , method :: Text
  , resourceType :: Text
  }
  deriving stock (Eq, Show)

-- | A context's requests so far, oldest first.
contextRequests :: Playwright -> Int -> IO (Seq Request)
contextRequests pw context = fromMaybe Seq.empty . IntMap.lookup context <$> readIORef pw.requestLogs

-- | A request of a context's that failed: refused, cut off, or aborted by a
-- route (Chromium's error text, such as @net::ERR_CONNECTION_REFUSED@).
data FailedRequest = FailedRequest
  { url :: Text
  , method :: Text
  , failure :: Text
  }
  deriving stock (Eq, Show)

-- | A context's failed requests, the last fifty, oldest first.
failedRequests :: Playwright -> Int -> IO (Seq FailedRequest)
failedRequests pw context = maybe Seq.empty (.requests) . IntMap.lookup context <$> readIORef pw.failures

-- | How a context's route handlers failed, the last fifty, oldest first:
-- each the request's address and the exception.
routeFailures :: Playwright -> Int -> IO (Seq Text)
routeFailures pw context = maybe Seq.empty (.handlers) . IntMap.lookup context <$> readIORef pw.failures

-- | Add to a context's failures, keeping the last fifty of each kind.
addFailure :: Playwright -> Int -> (ContextFailures -> ContextFailures) -> IO ()
addFailure pw context f =
  atomicModifyIORef' pw.failures (\m -> (IntMap.alter (Just . f . fromMaybe (ContextFailures Seq.empty Seq.empty)) context m, ()))

lastFifty :: Seq a -> Seq a
lastFifty xs = Seq.drop (Seq.length xs - 50) xs

-- | The driver's JavaScript, compiled in so that it is found wherever the
-- program runs: an installed library, a build directory, a test suite run
-- from another package's directory.
driverScript :: Text
driverScript =
  Text.pack
    $( do
         file <- TH.makeRelativeToProject "js/driver.mjs"
         TH.addDependentFile file
         source <- TH.runIO (readFile file)
         TH.lift source
     )

-- | Start node and Chromium, run the body with them, and end both, however
-- the body ends (an exception, a killed thread). A driver that does not
-- start (no @node@, no npm @playwright@, no Chromium for it) is a
-- 'PlaywrightError' with what node said.
withPlaywright :: Options -> (Playwright -> IO a) -> IO a
withPlaywright options body =
  withSystemTempDirectory "haskell-halogen-playwright" $ \dir -> do
    let script = dir </> "driver.mjs"
    ByteString.writeFile script (Text.encodeUtf8 driverScript)
    bracket (start dir script) stop $ \(pw, ready) -> do
      readMVar ready >>= either throwIO pure
      body pw
  where
    config dir =
      Text.unpack . Text.decodeUtf8 . LazyByteString.toStrict . Aeson.encode $
        Aeson.object
          [ "headless" .= options.headless
          , "slowMo" .= options.slowMotion
          , "timeout" .= options.timeout
          , "throttle" .= options.throttle
          , "args" .= options.browserArgs
          , -- Downloads go with the driver's directory.
            "downloadsPath" .= (dir </> "downloads")
          ]

    start dir script = mask_ $ do
      let config' =
            Process.setStdin Process.createPipe
              . Process.setStdout Process.createPipe
              . Process.setStderr Process.createPipe
              $ Process.proc options.node [script, config dir]
      started <- try (Process.startProcess config')
      process <- case started of
        Right p -> pure p
        Left (e :: SomeException) ->
          throwIO (PlaywrightError "start" "Error" ("cannot run " <> Text.pack options.node <> ": " <> Text.pack (displayException e)))
      let input = Process.getStdin process
      forM_ [input, Process.getStdout process, Process.getStderr process] $ \h -> hSetBinaryMode h True
      hSetBuffering input (BlockBuffering Nothing)
      writing <- newMVar ()
      pending <- newIORef IntMap.empty
      nextId <- newIORef 1
      gone <- newIORef Nothing
      logs <- newIORef IntMap.empty
      logged <- newTVarIO 0
      requestLogs <- newIORef IntMap.empty
      failures <- newIORef IntMap.empty
      routes <- newIORef IntMap.empty
      errorLines <- newIORef Seq.empty
      ready <- newEmptyMVar
      let pw = Playwright {options, input, process, writing, pending, nextId, gone, logs, logged, requestLogs, failures, routes, errorLines}
      stderrDone <- newEmptyMVar
      _ <- forkIO (relayErrors pw (Process.getStderr process) `finally` putMVar stderrDone ())
      _ <- forkIO (readReplies pw (Process.getStdout process) ready stderrDone)
      pure (pw, ready)

    -- Ask node to close the browser and leave (its stdin ending, which is
    -- also what node sees if this process is killed outright), give it a
    -- few seconds, then SIGTERM (on which the driver closes the browser
    -- too), then SIGKILL: a killed caller, a timeout, Ctrl-C must leave no
    -- node and no Chromium behind. Uninterruptible, so a second exception
    -- does not cut the cleanup short; every wait in it is bounded.
    stop (pw, _) = uninterruptibleMask_ $ do
      void (try (hClose pw.input) :: IO (Either SomeException ()))
      exited <- waitExit pw.process 50
      unless exited $ do
        signalDriver sigTERM pw
        terminated <- waitExit pw.process 30
        unless terminated $ do
          killDriver pw
          void (waitExit pw.process 50)
      -- Reaps the process (it has exited by now) and closes its pipes.
      void (try (Process.stopProcess pw.process) :: IO (Either SomeException ()))
      markGone pw "the driver was stopped (withPlaywright ended)"

    waitExit :: Process.Process Handle Handle Handle -> Int -> IO Bool
    waitExit process tries = do
      code <- Process.getExitCode process
      case code of
        Just _ -> pure True
        Nothing
          | tries <= 0 -> pure False
          | otherwise -> threadDelay 100_000 >> waitExit process (tries - 1)

-- | Kill node at once (which takes its Chromium with it). For a test of
-- what a dead driver does; 'withPlaywright' ends it by itself.
killDriver :: Playwright -> IO ()
killDriver = signalDriver sigKILL

signalDriver :: Signal -> Playwright -> IO ()
signalDriver signal pw =
  driverProcessId pw >>= \case
    Nothing -> pure ()
    Just pid -> void (try (signalProcess signal pid) :: IO (Either SomeException ()))

-- | node's process id, while it runs.
driverProcessId :: Playwright -> IO (Maybe ProcessID)
driverProcessId pw = getPid (Process.unsafeProcessHandle pw.process)

-- | Mark the driver unusable, fail every call waiting for a reply, and stop
-- every route handler under way: there is no one left to answer.
markGone :: Playwright -> Text -> IO ()
markGone pw why = do
  first <- atomicModifyIORef' pw.gone $ \case
    Nothing -> (Just why, True)
    already -> (already, False)
  when first $ do
    waiting <- atomicModifyIORef' pw.pending (IntMap.empty,)
    for_ waiting $ \reply -> tryPutMVar reply (Left (PlaywrightError "" "DriverGone" why))
    entries <- atomicModifyIORef' pw.routes (IntMap.empty,)
    stopHandlers (foldMap (\e -> IntMap.elems e.inFlight) entries)

-- | Kill these handlers' threads, from a thread of its own: 'killThread'
-- waits until the exception is delivered, and its caller may be the reader,
-- which must not wait.
stopHandlers :: [ThreadId] -> IO ()
stopHandlers threads = unless (null threads) $ void (forkIO (mapM_ killThread threads))

-- | Write a line to node, whole: masked, so that a thread killed while
-- writing does not leave half a message on the pipe. Only a failure to
-- write (the driver gone) is an exception here.
writeLine :: Playwright -> ByteString.ByteString -> IO ()
writeLine pw line = uninterruptibleMask_ $ withMVar pw.writing $ \() -> do
  ByteString.hPut pw.input (line <> "\n")
  hFlush pw.input

-- | node's stderr, relayed line by line, the last lines kept.
relayErrors :: Playwright -> Handle -> IO ()
relayErrors pw h = loop
  where
    loop = do
      eof <- hIsEOF h
      unless eof $ do
        line <- Text.decodeUtf8Lenient <$> Char8.hGetLine h
        hPutStrLn stderr (Text.unpack ("[playwright] " <> line))
        atomicModifyIORef' pw.errorLines (\ls -> (Seq.drop (Seq.length ls - 19) (ls Seq.|> line), ()))
        loop

-- | node's stdout: replies to calls, and events. When it ends, the driver
-- is gone, and why is what node last said on stderr.
readReplies :: Playwright -> Handle -> MVar (Either PlaywrightError ()) -> MVar () -> IO ()
readReplies pw h ready stderrDone = (forever readOne `catchEnd` pure ()) `finally` ended
  where
    catchEnd io fallback = (try io :: IO (Either SomeException ())) >>= either (const fallback) pure
    readOne = do
      eof <- hIsEOF h
      when eof $ throwIO (userErrorText "end of output")
      line <- Char8.hGetLine h
      unless (Char8.all (`elem` (" \t\r" :: String)) line) $
        case Aeson.decodeStrict line of
          Just (Object o) -> dispatch o
          _ -> hPutStrLn stderr ("[playwright] not a message: " <> Text.unpack (Text.decodeUtf8Lenient line))
    dispatch o
      | Just (Number _) <- KeyMap.lookup "id" o = reply o
      | Just (String event) <- KeyMap.lookup "event" o = onEvent event o
      | Just (Number _) <- KeyMap.lookup "ask" o = onAsk o
      | otherwise = hPutStrLn stderr ("[playwright] a message of an unknown kind: " <> show o)
    reply o = do
      let parsed = flip Aeson.parseMaybe o $ \v -> do
            i <- v .: "id"
            failure <- v .:? "error"
            case failure of
              Just e -> (i,) . Left <$> ((,) <$> e .: "name" <*> e .: "message")
              Nothing -> (i,) . Right <$> v .: "result"
      for_ parsed $ \(i, outcome) -> do
        waiting <- atomicModifyIORef' pw.pending (\m -> (IntMap.delete i m, IntMap.lookup i m))
        for_ waiting $ \r -> tryPutMVar r (either (\(n, m) -> Left (PlaywrightError "" n m)) Right outcome)
    onEvent event o = case event of
      "ready" -> void (tryPutMVar ready (Right ()))
      "fatal" -> do
        let why = fromMaybe "the driver failed" (Aeson.parseMaybe (.: "message") o)
        markGone pw why
        void (tryPutMVar ready (Left (PlaywrightError "start" "DriverFailed" why)))
      -- Relayed first, so that one a wait on the log has seen has been
      -- printed.
      "console" -> for_ (Aeson.parseMaybe (\v -> (,,) <$> v .: "page" <*> v .: "type" <*> v .: "text") o) $ \(page, kind, text) -> do
        relay page kind text
        modifyLog page (\l -> l {console = l.console Seq.|> ConsoleMessage {kind, text}})
      "pageerror" -> for_ (Aeson.parseMaybe (\v -> (,) <$> v .: "page" <*> v .: "message") o) $ \(page, text) -> do
        relay page "uncaught" text
        modifyLog page (\l -> l {errors = l.errors Seq.|> text})
      "dialog" -> for_ (Aeson.parseMaybe (\v -> (,,) <$> v .: "page" <*> v .: "type" <*> v .: "message") o) $ \(page, kind, message) ->
        modifyLog page (\l -> l {dialogs = l.dialogs Seq.|> Dialog {kind, message}})
      "request" -> for_ (Aeson.parseMaybe (\v -> (,,,) <$> v .: "context" <*> v .: "url" <*> v .: "method" <*> v .: "resourceType") o) $ \(context, url, method, resourceType) ->
        atomicModifyIORef' pw.requestLogs (\m -> (IntMap.alter (Just . (Seq.|> Request {url, method, resourceType}) . fromMaybe Seq.empty) context m, ()))
      "requestfailed" -> for_ (Aeson.parseMaybe (\v -> (,,,) <$> v .: "context" <*> v .: "url" <*> v .: "method" <*> v .: "failure") o) $ \(context, url, method, failure) ->
        addFailure pw context (\f -> f {requests = lastFifty (f.requests Seq.|> FailedRequest {url, method, failure})})
      "unrouted" -> for_ (Aeson.parseMaybe (.: "routes") o) $ \ids -> do
        entries <- atomicModifyIORef' pw.routes (\m -> (foldr IntMap.delete m ids, [e | i <- ids, Just e <- [IntMap.lookup i m]]))
        stopHandlers (concatMap (\e -> IntMap.elems e.inFlight) entries)
      "closed" -> pure ()
      _ -> hPutStrLn stderr ("[playwright] an event of an unknown kind: " <> Text.unpack event)
    modifyLog page f = do
      atomicModifyIORef' pw.logs (\m -> (IntMap.alter (Just . f . fromMaybe emptyLog) page m, ()))
      atomically (modifyTVar' pw.logged (+ 1))
    -- Written as it arrives when the options ask for it: stderr, like
    -- node's own lines, which nothing else waits on for long.
    relay :: Int -> Text -> Text -> IO ()
    relay page kind text =
      when pw.options.relayConsole $
        hPutStrLn stderr (Text.unpack ("[page " <> Text.pack (show page) <> "] [" <> kind <> "] " <> text))
    onAsk o = for_ (Aeson.parseMaybe (\v -> (,,) <$> v .: "ask" <*> v .: "route" <*> v .: "request") o) $ \(ask, route, request) ->
      startHandler pw ask route request
    ended = do
      -- What node said last explains why it went: wait (briefly) for its
      -- stderr to end too.
      _ <- timeoutMVar stderrDone
      code <- Process.getExitCode pw.process
      said <- Text.intercalate "\n" . foldr (:) [] <$> readIORef pw.errorLines
      let why =
            "the driver (node) exited"
              <> maybe "" (\c -> " (" <> exitText c <> ")") code
              <> if Text.null said then "" else ":\n" <> said
      markGone pw why
      void (tryPutMVar ready (Left (PlaywrightError "start" "DriverFailed" why)))
    exitText = \case
      ExitSuccess -> "status 0"
      ExitFailure n -> "status " <> Text.pack (show n)
    timeoutMVar m = do
      result <- newEmptyMVar
      t1 <- forkIO (readMVar m >> void (tryPutMVar result ()))
      t2 <- forkIO (threadDelay 1_000_000 >> void (tryPutMVar result ()))
      takeMVar result `finally` (killThread t1 >> killThread t2)

userErrorText :: Text -> IOError
userErrorText = userError . Text.unpack

-- | Call a method and wait for its reply. The driver being gone (it died,
-- or 'withPlaywright' has ended) is a 'PlaywrightError', as is the call
-- failing in node.
call :: (FromJSON a) => Playwright -> Text -> [Aeson.Pair] -> IO a
call pw method params = do
  value <- call_ pw method params
  case Aeson.fromJSON value of
    Aeson.Success a -> pure a
    Aeson.Error e -> throwIO (PlaywrightError method "DecodeError" (Text.pack e <> " in " <> Text.decodeUtf8Lenient (LazyByteString.toStrict (Aeson.encode value))))

-- | 'call', for the reply as JSON.
call_ :: Playwright -> Text -> [Aeson.Pair] -> IO Value
call_ pw method params = do
  i <- atomicModifyIORef' pw.nextId (\n -> (n + 1, n))
  reply <- newEmptyMVar
  let forget = atomicModifyIORef' pw.pending (\m -> (IntMap.delete i m, ()))
      line = LazyByteString.toStrict (Aeson.encode (Aeson.object (("id" .= i) : ("method" .= method) : params)))
  -- Registered before the driver is checked, so that one that goes in
  -- between fails this call too ('markGone' fails what is registered).
  atomicModifyIORef' pw.pending (\m -> (IntMap.insert i reply m, ()))
  outcome <-
    ( do
        readIORef pw.gone >>= \case
          Just why -> pure (Left (PlaywrightError method "DriverGone" why))
          Nothing -> do
            -- Only a failure to write means the driver is gone: an
            -- exception thrown to this thread (a test's timeout) is not
            -- one, and goes on to the caller, once the line is written
            -- whole.
            written <- try (writeLine pw line)
            case written of
              Left (e :: IOException) -> do
                why <- fromMaybe ("cannot write to the driver: " <> Text.pack (displayException e)) <$> readIORef pw.gone
                pure (Left (PlaywrightError method "DriverGone" why))
              Right () -> takeMVar reply
    )
      `onException` forget
  forget
  either (\e -> throwIO (PlaywrightError method e.name e.message)) pure outcome

----------------------------------------------------------------------
-- Routes

-- | Register a handler for a route and ask node to route a page's or a
-- context's requests matching a glob to it; the route's id. The handler is
-- given each request as node describes it, and returns the answer's
-- fields; it runs on a thread of its own for each request, so it may
-- block. The target is @"page" .= handle@ or @"context" .= handle@; the
-- context is the one whose failures a handler's failure is added to.
addRoute :: Playwright -> Int -> Aeson.Pair -> Text -> (Aeson.Object -> IO [Aeson.Pair]) -> IO Int
addRoute pw context target pattern handler = do
  route <- atomicModifyIORef' pw.nextId (\n -> (n + 1, n))
  -- Registered first, so that the first ask finds it.
  atomicModifyIORef' pw.routes (\m -> (IntMap.insert route RouteEntry {context, pattern, handler, inFlight = IntMap.empty} m, ()))
  (call_ pw "route" ["route" .= route, "context" .= context, target, "pattern" .= pattern] >> pure route)
    `onException` atomicModifyIORef' pw.routes (\m -> (IntMap.delete route m, ()))

-- | Take a route away: its handlers under way are stopped, and node lets
-- the requests they held go on, as if the route had not been there. On a
-- driver that has gone there is nothing left to do.
removeRoute :: Playwright -> Int -> IO ()
removeRoute pw route = do
  entry <- atomicModifyIORef' pw.routes (\m -> (IntMap.delete route m, IntMap.lookup route m))
  for_ entry $ \e -> stopHandlers (IntMap.elems e.inFlight)
  outcome <- try (call_ pw "unroute" ["route" .= route])
  case outcome of
    Left (e :: PlaywrightError) | e.name /= "DriverGone" -> throwIO e
    _ -> pure ()

-- | Answer an ask from a thread of its own, which is registered with its
-- route before it starts, so that removing the route finds it. A route
-- that has gone meanwhile lets the request go on.
startHandler :: Playwright -> Int -> Int -> Aeson.Object -> IO ()
startHandler pw ask route request = mask_ $ do
  registered <- newEmptyMVar
  thread <- forkIOWithUnmask $ \unmask -> do
    found <- readMVar registered
    case found of
      Nothing -> answer ["action" .= ("continue" :: Text)]
      Just entry -> run unmask entry `finally` deregister
  found <- atomicModifyIORef' pw.routes $ \m -> case IntMap.lookup route m of
    Just e -> (IntMap.insert route e {inFlight = IntMap.insert ask thread e.inFlight} m, Just e)
    Nothing -> (m, Nothing)
  putMVar registered found
  where
    answer fields = writeAnswer (LazyByteString.toStrict (Aeson.encode (Aeson.object (("answer" .= ask) : fields))))
    -- The answer is encoded inside the handler's 'try', so that an error
    -- hidden in it (a lazy body) is the handler's failure.
    run unmask entry = do
      outcome <- try (unmask (entry.handler request >>= \fields -> evaluate (LazyByteString.toStrict (Aeson.encode (Aeson.object (("answer" .= ask) : fields))))))
      case outcome of
        Right line -> writeAnswer line
        Left (e :: SomeException)
          -- Stopped: its route went, or the driver did; node has let the
          -- request go.
          | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
          | otherwise -> do
              let address = fromMaybe "?" (Aeson.parseMaybe (.: "url") request)
                  what = entry.pattern <> ": " <> address <> ": " <> Text.pack (displayException e)
              hPutStrLn stderr (Text.unpack ("[playwright] a route's handler failed, and its request was aborted: " <> what))
              addFailure pw entry.context (\f -> f {handlers = lastFifty (f.handlers Seq.|> what)})
              answer ["action" .= ("abort" :: Text), "error" .= ("failed" :: Text)]
    -- A driver that has gone cannot be answered, and needs no answer.
    writeAnswer line = void (try (writeLine pw line) :: IO (Either IOException ()))
    deregister = atomicModifyIORef' pw.routes $ \m ->
      (IntMap.adjust (\e -> e {inFlight = IntMap.delete ask e.inFlight}) route m, ())
