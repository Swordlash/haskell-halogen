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
--
-- A run whose arguments hold @--learn-spans@ (ours too) marks the elements
-- @Lesson@ builds with where they are in its source: each application of an
-- element function of "Halogen.HTML.Elements" to a literal list of
-- properties, such as @HH.button [HE.onClick (const Increment)] [...]@ at
-- line 30, column 7, gets one property more, the attribute
-- @data-learn-src="30:7"@ (see 'lessonSpans'). The tutorial passes it on
-- every run, and its page reads the attribute back from the elements the
-- program mounted, to tell the reader which line made the element under the
-- pointer.
module Playground (playground) where

import Control.Monad
import Control.Monad.Trans.State.Strict (State, put, runState)
import Data.Coerce
import Data.Data (Data, cast, gmapM)
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.List (find, intercalate, isSuffixOf, sortOn)
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
import GHC.Types.SourceText (SourceText (..))
import GHC.Types.TypeEnv (typeEnvIds)
import GHC.Unit.Env (ue_updateHomeUnitEnv)
import GHC.Unit.Home.Graph (HomeUnitEnv (..))
import GHC.Utils.Exception
import GHC.Utils.Fingerprint (Fingerprint, fingerprintString)
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
    -- What 'lessonSpans' marks with, found by the first run that asks.
    marksFound <- newIORef Nothing
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
              withSpans = spansFlag `elem` given
              -- Typechecking and nothing after it: no bytecode, and no
              -- linking (-fno-code sets -no-link too).
              args = filter (`notElem` [checkFlag, spansFlag]) given ++ ["-fno-code" | checkOnly]
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
            -- The plugin that keeps Lesson's types, and the one that marks
            -- its elements when the run asks for that, the session's only
            -- static ones: put in place before each load rather than once,
            -- so as not to depend on what setting the flags does to the
            -- session's plugins.
            marks <- if withSpans then spanMarks marksFound else pure Nothing
            modifySession $ \env ->
              env
                { hsc_plugins =
                    (hsc_plugins env)
                      { staticPlugins =
                          [ StaticPlugin (PluginWithArgs plugin []) True
                          | plugin <- lessonTypes typesFound : [lessonSpans m | Just m <- [marks]]
                          ]
                      }
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
    spansFlag = "--learn-spans"

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

-- | What 'lessonSpans' marks elements with: the names of the element
-- functions, and the import that brings the attribute into the module's
-- scope.
data SpanMarks = SpanMarks
  { markedElements :: OccSet
  , marksImport :: LImportDecl GhcPs
  }

-- | The marks, found once a session. The element functions are the ones
-- "Halogen.HTML.Elements" exports, read from its interface through the
-- package databases the run has, so that they are the library's own and
-- follow it. Nothing when the module cannot be found (a run without
-- @-package haskell-halogen-core@): the run then marks nothing, and goes on.
spanMarks :: IORef (Maybe SpanMarks) -> Ghc (Maybe SpanMarks)
spanMarks found = do
  kept <- liftIO $ readIORef found
  case kept of
    Just marks -> pure (Just marks)
    Nothing -> handleSourceError (const (pure Nothing)) $ do
      elementsModule <- lookupModule (mkModuleName "Halogen.HTML.Elements") Nothing
      exports <- maybe [] modInfoExports <$> getModuleInfo elementsModule
      decl <- parseImportDecl ("import qualified Halogen.HTML as " ++ moduleNameString spansQualifier ++ " (attr, AttrName (..))")
      let marks =
            SpanMarks
              { markedElements =
                  mkOccSet [occ | name <- exports, let occ = nameOccName name, isVarOcc occ, not ("_" `isSuffixOf` occNameString occ)]
              , -- Implicit, as the Prelude's import is: GHC then never warns
                -- of it, whatever warnings the reader asks for.
                marksImport = noLocA decl {ideclExt = (ideclExt decl) {ideclImplicit = True}}
              }
      liftIO $ writeIORef found (Just marks)
      pure (Just marks)

-- | The qualifier the marks' names are written with, which no reader's
-- module has a reason to use.
spansQualifier :: ModuleName
spansQualifier = mkModuleName "Learn__Spans"

-- | A plugin that rewrites @Lesson@ as GHC has parsed it, so that each
-- element it builds says where in the source it was built. An application
-- of an element function to a literal list of properties,
--
-- > HH.button [HE.onClick (const Increment)] [HH.text "+"]
--
-- at line 30, column 7, becomes
--
-- > HH.button [Learn__Spans.attr (Learn__Spans.AttrName "data-learn-src") "30:7", HE.onClick (const Increment)] [HH.text "+"]
--
-- and the module gets an import of "Halogen.HTML" qualified as
-- @Learn__Spans@ (only @attr@ and @AttrName@), whatever it imports itself.
-- The rewrite is of the syntax tree, whose every other node keeps its place
-- in the source, so GHC's diagnostics still point at the reader's lines.
--
-- A function counts as an element when its name is one that
-- "Halogen.HTML.Elements" exports and the module imports that name from
-- there or from "Halogen.HTML": qualified with the alias of such an import
-- (@HH.div@, for @import Halogen.HTML qualified as HH@), or unqualified when
-- such an import is not qualified-only, either way unless the import's list
-- leaves the name out. So @head [1, 2]@ is left alone in a module that
-- imports the elements qualified, and so is a function of the reader's own
-- named like an element where no import brings an element in under that
-- name. (A local binding that shadows an imported element is taken for it,
-- and GHC then reports the list's type.)
--
-- Left alone, and so without a mark: an element whose properties are not a
-- literal list (@HH.div props kids@, a list comprehension, a list joined
-- with @<>@), the @_@ variants (@HH.div_ kids@, with no list of properties
-- to add to), elements made with @HH.element@ or @HH.withKeys@, SVG's
-- ("Halogen.Svg.Elements"), and what an @hh@ quasi-quote makes, which is not
-- in the tree GHC parsed.
lessonSpans :: SpanMarks -> Plugin
lessonSpans marks =
  defaultPlugin
    { parsedResultAction = \_ summary parsed ->
        pure $
          if ms_mod_name summary == lessonName
            then parsed {parsedResultModule = markModule marks (parsedResultModule parsed)}
            else parsed
    , -- Whether the elements are marked is part of what the module compiles
      -- to: a run without the flag after one with it compiles Lesson again.
      pluginRecompile = \_ -> pure (MaybeRecompile (fingerprintString "learn-spans"))
    }

markModule :: SpanMarks -> HsParsedModule -> HsParsedModule
markModule marks parsed =
  parsed {hpm_module = L at (if changed then marked {hsmodImports = hsmodImports marked ++ [marksImport marks]} else marked)}
  where
    L at lesson = hpm_module parsed
    (marked, changed) = runState (markAll lesson) False
    imports =
      [ decl
      | L _ decl <- hsmodImports lesson
      , unLoc (ideclName decl) `elem` map mkModuleName ["Halogen.HTML", "Halogen.HTML.Elements"]
      ]

    -- Bottom-up over the whole module, so that the elements inside an
    -- element are marked too: every node's children first, then the node
    -- itself if it is an expression.
    markAll :: (Data a) => a -> State Bool a
    markAll node = do
      inner <- gmapM markAll node
      maybe pure id (cast markElement) inner

    markElement :: LHsExpr GhcPs -> State Bool (LHsExpr GhcPs)
    markElement e = case e of
      L l (HsApp x f@(L _ (HsVar _ (L _ name))) (L lp props))
        | isElement name
        , Just (ann, items) <- literalList props
        , RealSrcSpan from _ <- locA l -> do
            put True
            pure $ L l $ HsApp x f $ L lp $ ExplicitList ann (mark from : items)
      _ -> pure e

    -- An empty list is the constructor [], as GHC parses it, not an empty
    -- ExplicitList.
    literalList props = case props of
      ExplicitList ann items -> Just (ann, items)
      HsVar _ (L _ name) | name == getRdrName nilDataCon -> Just (noAnn, [])
      _ -> Nothing

    isElement name = rdrNameOcc name `elemOccSet` markedElements marks && any (brings name) imports
    brings name decl =
      lists (rdrNameOcc name) decl && case isQual_maybe name of
        Just (qualifier, _) -> qualifier == maybe (unLoc (ideclName decl)) unLoc (ideclAs decl)
        Nothing -> ideclQualified decl == NotQualified
    lists occ decl = case ideclImportList decl of
      Nothing -> True
      Just (Exactly, L _ entries) -> occ `elem` named entries
      Just (EverythingBut, L _ entries) -> occ `notElem` named entries
    named entries = [rdrNameOcc n | L _ entry <- entries, n <- ieNames entry]

    mark from =
      nlHsApp
        (nlHsApp (nlHsVar (qualified (mkVarOcc "attr"))) (nlHsPar (nlHsApp (nlHsVar (qualified (mkDataOcc "AttrName"))) (string "data-learn-src"))))
        (string (show (srcSpanStartLine from) ++ ":" ++ show (srcSpanStartCol from)))
    qualified = mkRdrQual spansQualifier
    -- A string literal as OverloadedStrings reads one, so that it is the
    -- Text the attribute takes whether or not the module has the extension.
    string = noLocA . HsOverLit noExtField . mkHsIsString NoSourceText . mkFastString

foreign import javascript "wrapper"
  toRun :: Run -> IO (JSFunction Run)

foreign export javascript "playground"
  playground :: JSString -> IO (JSFunction Run)
