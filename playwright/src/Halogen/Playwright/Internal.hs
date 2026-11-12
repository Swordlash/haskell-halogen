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
-- not a reply is told apart by its keys (an @event@ today).
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

    -- * What pages said, and what contexts asked for
  , ConsoleMessage (..)
  , PageLog (..)
  , pageLog
  , Request (..)
  , contextRequests

    -- * The driver's source
  , driverScript
  )
where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (Exception (..), IOException, SomeException, bracket, finally, mask_, onException, throwIO, try, uninterruptibleMask_)
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
import Data.Maybe (fromMaybe)
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
  }
  deriving stock (Show)

-- | Headless, at full speed, five seconds, the @node@ on the path, no
-- screenshots.
defaultOptions :: Options
defaultOptions = Options {headless = True, slowMotion = 0, timeout = 5000, node = "node", artifacts = Nothing}

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
  , requestLogs :: IORef (IntMap (Seq Request))
  -- ^ Each context's requests, oldest first.
  , errorLines :: IORef (Seq Text)
  -- ^ The last lines node wrote to stderr, for the message when it dies.
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

-- | What a page has said, oldest first.
data PageLog = PageLog
  { console :: Seq ConsoleMessage
  , errors :: Seq Text
  -- ^ Uncaught exceptions and unhandled rejections, with their stacks.
  }

emptyLog :: PageLog
emptyLog = PageLog Seq.empty Seq.empty

-- | What a page has said so far.
pageLog :: Playwright -> Int -> IO PageLog
pageLog pw page = fromMaybe emptyLog . IntMap.lookup page <$> readIORef pw.logs

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
    bracket (start script) stop $ \(pw, ready) -> do
      readMVar ready >>= either throwIO pure
      body pw
  where
    config =
      Text.unpack . Text.decodeUtf8 . LazyByteString.toStrict . Aeson.encode $
        Aeson.object
          [ "headless" .= options.headless
          , "slowMo" .= options.slowMotion
          , "timeout" .= options.timeout
          ]

    start script = mask_ $ do
      let config' =
            Process.setStdin Process.createPipe
              . Process.setStdout Process.createPipe
              . Process.setStderr Process.createPipe
              $ Process.proc options.node [script, config]
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
      requestLogs <- newIORef IntMap.empty
      errorLines <- newIORef Seq.empty
      ready <- newEmptyMVar
      let pw = Playwright {options, input, process, writing, pending, nextId, gone, logs, requestLogs, errorLines}
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

-- | Mark the driver unusable, and fail every call waiting for a reply.
markGone :: Playwright -> Text -> IO ()
markGone pw why = do
  first <- atomicModifyIORef' pw.gone $ \case
    Nothing -> (Just why, True)
    already -> (already, False)
  when first $ do
    waiting <- atomicModifyIORef' pw.pending (IntMap.empty,)
    for_ waiting $ \reply -> tryPutMVar reply (Left (PlaywrightError "" "DriverGone" why))

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
      "console" -> for_ (Aeson.parseMaybe (\v -> (,,) <$> v .: "page" <*> v .: "type" <*> v .: "text") o) $ \(page, kind, text) ->
        modifyLog page (\l -> l {console = l.console Seq.|> ConsoleMessage {kind, text}})
      "pageerror" -> for_ (Aeson.parseMaybe (\v -> (,) <$> v .: "page" <*> v .: "message") o) $ \(page, text) ->
        modifyLog page (\l -> l {errors = l.errors Seq.|> text})
      "request" -> for_ (Aeson.parseMaybe (\v -> (,,,) <$> v .: "context" <*> v .: "url" <*> v .: "method" <*> v .: "resourceType") o) $ \(context, url, method, resourceType) ->
        atomicModifyIORef' pw.requestLogs (\m -> (IntMap.alter (Just . (Seq.|> Request {url, method, resourceType}) . fromMaybe Seq.empty) context m, ()))
      "closed" -> pure ()
      _ -> hPutStrLn stderr ("[playwright] an event of an unknown kind: " <> Text.unpack event)
    modifyLog page f = atomicModifyIORef' pw.logs (\m -> (IntMap.alter (Just . f . fromMaybe emptyLog) page m, ()))
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
            -- one, and goes on to the caller. The line and its end go
            -- in one write.
            written <- try $ withMVar pw.writing $ \() -> do
              ByteString.hPut pw.input (line <> "\n")
              hFlush pw.input
            case written of
              Left (e :: IOException) -> do
                why <- fromMaybe ("cannot write to the driver: " <> Text.pack (displayException e)) <$> readIORef pw.gone
                pure (Left (PlaywrightError method "DriverGone" why))
              Right () -> takeMVar reply
    )
      `onException` forget
  forget
  either (\e -> throwIO (PlaywrightError method e.name e.message)) pure outcome
