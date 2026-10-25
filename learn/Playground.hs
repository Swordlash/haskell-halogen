-- | GHC itself, as a shared library the page loads with GHC's own dynamic
-- linker (@dyld.mjs@): it compiles the program the page hands it to
-- bytecode and runs its @Main.main@, in the page, with no server.
--
-- This follows GHC's own browser playground (the testsuite's
-- @ghc-api-browser/playground001.hs@). One session is shared by every run, so
-- what the first run loaded (the boot libraries and the packages the
-- program's @-package@ flags name) stays loaded for the next, and a run is
-- only the compilation of the program itself.
module Playground (playground) where

import Control.Monad
import Data.Coerce
import Data.IORef
import GHC
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Monad
import GHC.Plugins
import GHC.Runtime.Interpreter
import GHC.Unit.Env (ue_updateHomeUnitEnv)
import GHC.Unit.Home.Graph (HomeUnitEnv (..))
import GHC.Utils.Exception
import GHC.Wasm.Prim
import System.IO (BufferMode (..), hFlush, hSetBuffering, stderr, stdout)

newtype JSFunction t = JSFunction JSVal

-- | What the page calls for a run: the GHC arguments, as one string split at
-- spaces, and the source of @Main@.
type Run = JSString -> JSString -> IO ()

-- | Start a session on the libdir at this path (in the page's in-memory file
-- system) and give back the asynchronous JavaScript function that runs a
-- program in it.
playground :: JSString -> IO (JSFunction Run)
playground jsLibdir =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    libdir <- evaluate $ fromJSString jsLibdir
    freeJSVal $ coerce jsLibdir
    -- The page collects what a run writes while the run is under way, a
    -- line at a time, and stops when the run returns. The handles are the
    -- program's too (it is linked against the same base), and stdout is
    -- block-buffered on a file descriptor that is not a terminal, so a
    -- program's putStrLn would reach the page only when the buffer filled
    -- or something flushed it, in this run or a later one, or never.
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    -- Not runGhc, which would end the session when it returns: the session
    -- outlives this call, for every run to reuse.
    session <- Session <$> newIORef undefined
    -- The flags every run starts from, so that what one run's arguments
    -- set does not carry over into the next.
    dflags0 <- flip reflectGhc session $ do
      initGhcMonad (Just libdir)
      dflags <- getSessionDynFlags
      void
        $ setSessionDynFlags
        $ dflags
          { ghcMode = CompManager
          , backend = interpreterBackend
          , ghcLink = LinkInMemory
          , verbosity = 1
          }
      getSessionDynFlags
    -- Each call runs on a thread of its own. The session is not safe to use
    -- from two at once; the page lets one run start only once the last has
    -- returned.
    --
    -- Both handles are flushed once the run is over, whether it ended or
    -- failed (after GHC's error handler has written what it failed of), for
    -- a hPutStr without a newline.
    toRun $ \jsArgs jsSrc ->
      flip finally (hFlush stdout >> hFlush stderr) $
        defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
          args <- evaluate $ words $ fromJSString jsArgs
          freeJSVal $ coerce jsArgs
          writeFile source $ fromJSString jsSrc
          freeJSVal $ coerce jsSrc
          -- withCleanupSession only removes the run's temporary files, so the
          -- session stays usable after it.
          flip reflectGhc session $ withCleanupSession $ do
            void $ setSessionDynFlags dflags0
            logger0 <- getLogger
            (dflags1, _, warnings) <- parseDynamicFlags logger0 dflags0 $ map noLoc args
            -- The session keeps the package databases it read first and does
            -- not read them again, so a -package-db these arguments add (a
            -- layer the page extracted since) would go unseen. Forget them,
            -- for setSessionDynFlags to read the ones these flags name.
            modifySession $ \env ->
              env
                { hsc_unit_env =
                    ue_updateHomeUnitEnv
                      (\home -> home {homeUnitEnv_unit_dbs = Nothing})
                      (hscActiveUnitId env)
                      (hsc_unit_env env)
                }
            void $ setSessionDynFlags dflags1
            logger1 <- getLogger
            liftIO
              $ printOrThrowDiagnostics logger1 (initPrintConfig dflags1) (initDiagOpts dflags1)
              $ GhcDriverMessage <$> warnings
            setTargets . pure =<< guessTarget source Nothing Nothing
            loaded <- load LoadAllTargets
            when (failed loaded) $ fail "the program did not compile"
            setContext [IIDecl $ simpleImportDecl $ mkModuleName "Main"]
            main <- compileExprRemote "Main.main"
            env <- getSession
            liftIO $ evalIO (hscInterp env) main
  where
    source = "/tmp/Main.hs"

foreign import javascript "wrapper"
  toRun :: Run -> IO (JSFunction Run)

foreign export javascript "playground"
  playground :: JSString -> IO (JSFunction Run)
