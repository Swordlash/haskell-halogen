-- | GHC itself, as a shared library the page loads with GHC's own dynamic
-- linker (@dyld.mjs@): it compiles the program the page hands it to
-- bytecode and runs its @Main.main@, in the page, with no server.
--
-- This follows GHC's own browser playground (the testsuite's
-- @ghc-api-browser/playground001.hs@). One session is shared by every run, so
-- what the first run loaded (the boot libraries and the packages the
-- program's @-package@ flags name) stays loaded for the next, and a run is
-- only the compilation of the program itself.
--
-- A run whose arguments hold @--learn-check@ (ours, not GHC's) only
-- type-checks the program: it is compiled with @-fno-code@, and its @main@ is
-- not run. The tutorial's page runs one as the reader types. Either way, once
-- the program has compiled, the run writes the types of the top-level
-- bindings of the module @Lesson@ to stdout, before what the program writes
-- there, one line each, its fields separated by tabs:
--
-- > learn-type<TAB><line><TAB><column><TAB><name><TAB><type>
module Playground (playground) where

import Control.Monad
import Data.Coerce
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.List (find, intercalate, sortOn)
import GHC
import GHC.Core.TyCo.Ppr (pprSigmaType)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Monad
import GHC.Plugins
import GHC.Runtime.Interpreter
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Tc.Utils.Monad qualified as Tc
import GHC.Types.TypeEnv (typeEnvIds)
import GHC.Unit.Env (ue_updateHomeUnitEnv)
import GHC.Unit.Home.Graph (HomeUnitEnv (..))
import GHC.Utils.Exception
import GHC.Utils.Fingerprint (Fingerprint)
import GHC.Wasm.Prim
import System.FilePath (takeFileName)
import System.IO (BufferMode (..), hFlush, hSetBuffering, stderr, stdout)

newtype JSFunction t = JSFunction JSVal

-- | What the page calls for a run: the GHC arguments, as one string split at
-- spaces (@--learn-check@ among them for a check), and the source of @Main@.
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
    -- The types of Lesson's bindings, as the last typecheck of it found
    -- them (see lessonTypes).
    typesFound <- newIORef Nothing
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
          given <- evaluate $ words $ fromJSString jsArgs
          freeJSVal $ coerce jsArgs
          let checkOnly = checkFlag `elem` given
              -- Typechecking and nothing after it: no bytecode, and no
              -- linking (-fno-code sets -no-link too).
              args = filter (/= checkFlag) given ++ ["-fno-code" | checkOnly]
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
            -- The plugin that keeps Lesson's types, the session's only
            -- static one: put in place before each load rather than once, so
            -- as not to depend on what setting the flags does to the
            -- session's plugins.
            modifySession $ \env ->
              env
                { hsc_plugins =
                    (hsc_plugins env) {staticPlugins = [StaticPlugin (PluginWithArgs (lessonTypes typesFound) []) True]}
                }
            setTargets . pure =<< guessTarget source Nothing Nothing
            loaded <- load LoadAllTargets
            when (failed loaded) $ fail "the program did not compile"
            reportTypes typesFound
            unless checkOnly $ do
              setContext [IIDecl $ simpleImportDecl $ mkModuleName "Main"]
              main <- compileExprRemote "Main.main"
              env <- getSession
              liftIO $ evalIO (hscInterp env) main
  where
    source = "/tmp/Main.hs"
    checkFlag = "--learn-check"

-- | The module whose bindings' types a run reports.
lessonName :: ModuleName
lessonName = mkModuleName "Lesson"

-- | The @learn-type@ lines of @Lesson@, and the hash of the source they are
-- of.
type Types = IORef (Maybe (Fingerprint, [String]))

-- | A plugin that keeps the types of @Lesson@'s top-level bindings each time
-- GHC has typechecked it, as the lines 'reportTypes' writes.
--
-- They are read from the typechecker's result rather than from the module's
-- interface once it is loaded, because that keeps only what the module
-- exports under @-fno-code@ (the interpreter keeps every binding, for
-- GHCi's sake), and in the module's own scope, so that a type reads as the
-- module would write it: @H.Component@, for @import Halogen qualified as H@.
--
-- What GHC makes beside the module's own bindings is left out (record
-- selectors, class methods, dictionary functions, constructors, the
-- @$@-named), and so are bindings under a @where@ or a @let@, which are not
-- top-level. Each type is printed as GHCi's @:type@ prints it, on one line.
lessonTypes :: Types -> Plugin
lessonTypes found =
  defaultPlugin
    { typeCheckResultAction = \_ summary env -> do
        when (ms_mod_name summary == lessonName) $ keep summary env
        pure env
    , pluginRecompile = purePlugin
    }
  where
    keep :: ModSummary -> TcGblEnv -> TcM ()
    keep summary env = do
      pprCtx <- Tc.getNamePprCtx
      dflags <- getDynFlags
      let ctx = initSDocContext dflags (mkUserStyle pprCtx AllTheWay)
          bindings =
            sortOn
              fst
              [ ((srcSpanStartLine at, srcSpanStartCol at), (getOccString ident, showSDocOneLine ctx (pprSigmaType (idType ident))))
              | ident <- typeEnvIds (tcg_type_env env)
              , not (isImplicitId ident || isRecordSelector ident || isDFunId ident)
              , take 1 (getOccString ident) /= "$"
              , RealSrcSpan at _ <- [nameSrcSpan (idName ident)]
              , takeFileName (unpackFS (srcSpanFile at)) == "Lesson.hs"
              ]
          rendered = [intercalate "\t" ["learn-type", show row, show column, binder, ty] | ((row, column), (binder, ty)) <- bindings]
      -- Printed now, so as not to keep the typechecker's environment.
      _ <- liftIO $ evaluate $ sum $ map length rendered
      liftIO $ writeIORef found $ Just (ms_hs_hash summary, rendered)

-- | Write the types of @Lesson@'s bindings, if the program has the module.
--
-- When GHC has not typechecked it again in this run, it is the same source
-- as when it last did (the source's hash is GHC's first test), so the types
-- kept then are its types; the hash is compared all the same.
reportTypes :: Types -> Ghc ()
reportTypes found = do
  graph <- getModuleGraph
  kept <- liftIO $ readIORef found
  for_ (find ((== lessonName) . ms_mod_name) (mgModSummaries graph)) $ \summary ->
    for_ kept $ \(hash, rendered) ->
      when (hash == ms_hs_hash summary) $ liftIO $ traverse_ putStrLn rendered

foreign import javascript "wrapper"
  toRun :: Run -> IO (JSFunction Run)

foreign export javascript "playground"
  playground :: JSString -> IO (JSFunction Run)
