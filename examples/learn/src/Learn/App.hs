-- | The tutorial: an introduction with the list of lessons, and a page per
-- lesson with its text, a code editor and the result of compiling the code,
-- which GHC does in the page ("Learn.Browser").
--
-- The playground is one more such page, with no lesson: the reader checks
-- the layers their code needs instead of a lesson's header naming them
-- ("Learn.Playground"). What the editor holds the code of, and what a
-- compile of it passes, is a 'Code', the lesson's or the playground's, so
-- that everything else here does not tell the two apart.
--
-- The build renders every route's page with this component natively
-- (@Prerender.hs@), so a lesson's text is on the page before any of the
-- program has arrived; the program then adopts that page
-- ('Halogen.VDom.Driver.hydrateUI'). What needs the browser (the editor,
-- the compiler, the reader's saved code) starts in 'Initialize', and only in
-- a browser ('inBrowser'): the prerender leaves it all out, and the HTML it
-- writes is this component's first render in the browser too.
--
-- The compiler is large (tens of megabytes to download, over a gigabyte of
-- memory once running), so on a narrow screen it loads only when the reader
-- asks; elsewhere it starts loading as the page does.
--
-- As the reader types, the page has GHC typecheck the code without running
-- it ('Browser.typecheck', a check), a moment after the last key, and shows
-- what GHC says in the editor and under it, with the types of the module's
-- top-level bindings: over their names, and in a list under the editor.
-- The Compile button still compiles and runs; a compile reports the types
-- too, and a check never touches the result.
--
-- What a compile mounts goes into @#learn-result@, an element this
-- component renders once per lesson (the lesson's page is keyed by its
-- slug) and never patches inside: its content is the compiled program's,
-- another Halogen tree, run by the compiler's runtime. Before the page takes
-- the element away (another route), that tree is ended ('endRun').
--
-- The Source toggle, once a compile has mounted something, shows which line
-- made what: the compiler marks every element of the reader's code with
-- where it was built (the playground's @--learn-spans@, passed on every
-- compile), and while Source is on, the element under the pointer is
-- outlined and its line marked in the editor, a click on it goes to that
-- line (the program does not get the click), and the element the editor's
-- cursor is in is outlined ('Browser.startSource').
module Learn.App (component) where

import Control.Monad.UUID (MonadUUID)
import DOM.HTML.Indexed qualified as I
import Data.List (lookup)
import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (inBrowser)
import Halogen.Router (Follow, Router)
import Halogen.Router qualified as Router
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MonadBrowserDOM, StorageKind (..), readStorageItem, removeStorageItem, writeStorageItem)
import Learn.Browser (CompileRequest (..), Editor, EditorEvent (..), Layer (..), Progress (..), RunResult (..), Sizes (..), Source, SourceEvent (..))
import Learn.Browser qualified as Browser
import Learn.Diagnostics (Diagnostic (..), Severity (..), didNotCompile, parseDiagnostics)
import Learn.Lesson (Lesson (..), defaultMain, wrapper)
import Learn.Lessons (lessons)
import Learn.Playground (Handoff (..))
import Learn.Playground qualified as Playground
import Learn.Route (Route (..), lessonOf)
import Learn.Types (Binding (..), displayName)
import Learn.View qualified as View
import Numeric (showFFloat)
import Protolude hiding (State, link)
import UnliftIO (MonadUnliftIO, tryAny)

data State = State
  { router :: Router Route
  , route :: Route
  , editorHost :: H.RefLabel
  , editor :: Maybe (Editor, H.SubscriptionId)
  , resultHost :: H.RefLabel
  -- ^ @#learn-result@, which the Source mode reads.
  , source :: Maybe (Source, H.SubscriptionId)
  -- ^ The Source mode, while it is on.
  , narrow :: Bool
  , compiler :: Compiler
  , sizes :: Maybe Sizes
  , loadedLayers :: [Text]
  , loadingLayers :: Bool
  -- ^ A layer load is under way; the lesson's layers are checked again
  -- when it ends, in case the reader has moved to another lesson since.
  , layerFailure :: Maybe Text
  -- ^ Why this lesson's layers did not load: the compiler, and the lessons
  -- that do not need them, are usable all the same.
  , runs :: Int
  -- ^ How many compiles have started, shown on the outcome as @data-run@
  -- so that a test can tell a new outcome from the last one.
  , visit :: Int
  -- ^ Counts the lessons entered: a compile started in one visit must not
  -- report into a later one, even of the same lesson, whose editor is new.
  , knownMb :: Double
  -- ^ How many megabytes the compiler reported loaded when it was last
  -- ready, for a Retry that finds it already started and gets no new report.
  , edited :: Bool
  -- ^ The editor's text is not the lesson's @Start.hs@.
  , run :: Run
  , mounted :: Bool
  -- ^ A compile has mounted something into this lesson's @#learn-result@.
  , diagnostics :: [Diagnostic]
  -- ^ What GHC said of the code the last time it was asked, by a compile
  -- or a check.
  , bindings :: Maybe Bindings
  , edits :: Int
  -- ^ Counts the editor's changes, and the lessons entered: a check's
  -- result is for the code it checked, and is dropped once there has been
  -- another change (or a compile, which says it all again, later).
  , coveredEdit :: Maybe Int
  -- ^ The 'edits' the last check or compile started from: the code then
  -- needs no check of its own.
  , checking :: Bool
  -- ^ A check is under way: one at a time.
  , checkDue :: Maybe H.ForkId
  -- ^ The check that waits for the reader to stop typing ('checkDelay'),
  -- killed by the next change while it waits.
  , lastCheck :: Maybe (Int, Bool)
  -- ^ How long the last check whose result is shown took, and whether the
  -- code typechecked.
  , playground :: PlaygroundSettings
  , handoff :: Maybe Handoff
  -- ^ What a lesson's Open in playground hands the playground, until the
  -- playground opens with it.
  }

-- | What the reader chose in the playground beside the code: the layers
-- checked, and GHC arguments of their own, as they typed them.
data PlaygroundSettings = PlaygroundSettings
  { layers :: [Text]
  , extra :: Text
  }

-- | What the editor is for, on the page the reader is at: a lesson's code,
-- or the playground's. A compile passes its layers and arguments.
data Code = Code
  { key :: Text
  -- ^ What the reader's code is kept under in @localStorage@.
  , start :: Text
  -- ^ What the editor opens with when nothing is kept, and what Reset puts
  -- back: a lesson's @Start.hs@, the playground's template.
  , solution :: Maybe Text
  , main :: Text
  -- ^ The program around @Lesson@.
  , layers :: [Text]
  , args :: [Text]
  }

codeOf :: State -> Maybe Code
codeOf st = case st.route of
  Playground ->
    Just
      Code
        { key = Playground.codeKey
        , start = Playground.template
        , solution = Nothing
        , main = defaultMain
        , layers = st.playground.layers
        , args = packages <> T.words st.playground.extra
        }
  r -> lessonCode <$> lessonOf r
  where
    -- Known once the manifest has been read; until then the page compiles
    -- nothing that needs them ('canCompile').
    packages = maybe [] (\s -> Playground.packageArgs s.basePackages [(l.name, l.packages) | l <- s.layers] st.playground.layers) st.sizes
    lessonCode lesson =
      Code
        { key = storageKey lesson
        , start = lesson.start
        , solution = Just lesson.solution
        , main = wrapper lesson
        , layers = lesson.layers
        , args = lesson.args
        }

-- | The types of the lesson's top-level bindings, from the last check or
-- compile that typechecked, and whether that is still so of the code (it is
-- not when the code has an error since).
data Bindings = Bindings
  { list :: [Binding]
  , current :: Bool
  }

data Compiler
  = NotLoaded
  | Loading Progress
  | -- | Ready, with this many megabytes loaded.
    Loaded Double
  | Broken Text

data Run
  = NotRun
  | Compiling
  | Ran RunResult
  | -- | The compiler could not be asked at all.
    Unrun Text

data Action
  = Initialize
  | Finalize
  | Arrived Route
  | Followed (Follow Route)
  | Progressed Progress
  | LoadCompiler
  | FromEditor EditorEvent
  | Compile
  | Reset
  | ShowSolution
  | Goto Int Int
  | ToggleSource
  | FromSource SourceEvent
  | OpenInPlayground
  | ToggleLayer Text Bool
  | SetExtraArgs Text

component :: forall q o m. (MonadBrowserDOM m, MonadUnliftIO m, MonadUUID m) => H.Component q (Router Route) o m
component =
  H.component
    initialState
    render
    [H.onAction handleAction, H.onInitialize (handleAction Initialize), H.onFinalize (handleAction Finalize)]
  where
    initialState router = do
      route <- Router.current router
      editorHost <- H.newRefLabel "editor"
      resultHost <- H.newRefLabel "result"
      pure
        State
          { router
          , route
          , editorHost
          , editor = Nothing
          , resultHost
          , source = Nothing
          , narrow = False
          , compiler = NotLoaded
          , sizes = Nothing
          , loadedLayers = []
          , loadingLayers = False
          , layerFailure = Nothing
          , runs = 0
          , visit = 0
          , knownMb = 0
          , edited = False
          , run = NotRun
          , mounted = False
          , diagnostics = []
          , bindings = Nothing
          , edits = 0
          , coveredEdit = Nothing
          , checking = False
          , checkDue = Nothing
          , lastCheck = Nothing
          , playground = PlaygroundSettings {layers = [], extra = ""}
          , handoff = Nothing
          }

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Initialize -> when inBrowser $ do
        router <- gets (.router)
        void $ H.subscribe (Arrived <$> Router.changes router)
        narrow <- H.liftEffect (liftIO Browser.narrowScreen)
        modify $ \s -> s {narrow}
        enterPage
        -- What the compiler weighs, for the page to say, whether or not it
        -- is loaded: the manifest is small.
        void $ H.fork readSizes
        unless narrow loadCompiler
      Finalize -> leavePage
      Arrived r -> do
        current <- gets (.route)
        unless (r == current) $ do
          leavePage
          modify $ \s ->
            s {route = r, edited = False, run = NotRun, mounted = False, layerFailure = Nothing, diagnostics = [], bindings = Nothing, lastCheck = Nothing}
          H.liftEffect (liftIO Browser.scrollToTop)
          enterPage
          gets (.compiler) >>= \case
            Loaded _ -> loadLayers >> startCheck
            _ -> pass
      Followed f -> do
        router <- gets (.router)
        Router.follow router f
      Progressed p -> do
        modify $ \s -> case (s.compiler, p) of
          (_, Ready mb) -> s {compiler = Loaded mb, knownMb = mb}
          (_, LoadFailed message) -> s {compiler = Broken message}
          -- A download that outlives a failed start goes on reporting; it
          -- must not hide the failure, and Retry with it.
          (Broken _, _) -> s
          _ -> s {compiler = Loading p}
        syncEditor
      LoadCompiler -> loadCompiler
      FromEditor (Changed text) -> withCode $ \code -> do
        -- What is kept is the reader's own code: none, once it is the
        -- lesson's start again (Reset).
        -- Storage the browser denies (a strict private window) must not
        -- stop the lesson: the code is then kept for this visit only.
        void
          $ H.liftEffect
          $ tryAny
          $ if text == code.start
            then removeStorageItem LocalStorage code.key
            else writeStorageItem LocalStorage code.key text
        let edited = text /= code.start
        was <- gets (.edited)
        when (was /= edited) $ modify $ \s -> s {edited}
        checkSoon
      FromEditor CompileRequested -> handleAction Compile
      FromEditor (Cursor line column) ->
        gets (.source) >>= traverse_ \(source, _) -> H.liftEffect (liftIO (Browser.outlineSource source line column))
      Compile -> do
        st <- get
        when (canCompile st) $ for_ ((,) <$> codeOf st <*> st.editor) $ \(code, (editor, _)) -> do
          source <- H.liftEffect (liftIO (Browser.compileText editor))
          modify $ \s -> s {run = Compiling, runs = s.runs + 1, coveredEdit = Just s.edits}
          compiled <- liftIO $ Browser.compile (request code source)
          -- The reader may have left meanwhile, even for this same lesson
          -- again: that visit's editor is another one, and its run its own.
          now <- gets (.visit)
          when (now == st.visit) $ do
            case compiled of
              Left message -> modify $ \s -> s {run = Unrun message}
              Right result0 -> do
                let diagnostics = parseDiagnostics result0.stderr
                    -- What GHC wrote wins over how the run ended: see
                    -- 'didNotCompile'.
                    result = result0 {compiled = result0.compiled && (result0.ok || not (didNotCompile result0.stderr))}
                markDiagnostics editor diagnostics
                -- The types were written before main ran: a program that
                -- compiled and then stopped has them too.
                keepTypes editor result.compiled result.types
                modify $ \s -> s {run = Ran result, mounted = s.mounted || result.compiled, diagnostics, lastCheck = Nothing}
                -- The result's marks are positions in this compile's code
                -- now, which the editor maps to where that code is as the
                -- reader edits on. What the Source mode outlined was the
                -- last run's: the line marked for the pointer is forgotten,
                -- and the editor says where its cursor is again, for the new
                -- elements.
                when result.compiled $ do
                  H.liftEffect (liftIO (Browser.markCompiled editor))
                  gets (.source) >>= traverse_ \_ -> H.liftEffect $ liftIO $ do
                    Browser.showSource editor 0
                    Browser.setSourceMode editor True
            -- The reader may have typed while it compiled.
            startCheck
      Reset -> withEditor $ \code editor -> H.liftEffect (liftIO (Browser.setEditorText editor code.start))
      ShowSolution -> withEditor $ \code editor -> for_ code.solution $ \solution -> do
        edited <- gets (.edited)
        sure <-
          if edited
            then H.liftEffect (liftIO (Browser.confirm "Replace your code with the lesson's solution? Your code is not kept."))
            else pure True
        when sure $ H.liftEffect (liftIO (Browser.setEditorText editor solution))
      -- The lesson's code, as the reader has it, with what it compiles
      -- with, for the playground to open with when it is entered. It stays
      -- in this component's state: the playground is entered without a
      -- page load, and the browser's storage may refuse to keep it.
      OpenInPlayground -> withEditor $ \code editor -> do
        text <- H.liftEffect (liftIO (Browser.editorText editor))
        modify $ \s -> s {handoff = Just Handoff {code = text, layers = code.layers, args = code.args}}
        router <- gets (.router)
        Router.navigate router Playground
      ToggleLayer name checked -> do
        modify $ \s ->
          s
            { playground =
                PlaygroundSettings
                  { layers = (if checked then ordNub . (<> [name]) else filter (/= name)) s.playground.layers
                  , extra = s.playground.extra
                  }
            , layerFailure = Nothing
            }
        keepSettings
        gets (.compiler) >>= \case
          Loaded _ -> loadLayers
          _ -> pass
        -- The arguments are not the ones the last check had.
        checkSoon
      SetExtraArgs extra -> do
        modify $ \s -> s {playground = s.playground {extra}}
        keepSettings
        checkSoon
      Goto line column -> withEditor $ \_ editor -> H.liftEffect (liftIO (Browser.gotoPosition editor line column))
      ToggleSource -> gets (.source) >>= maybe startSource (const stopSource)
      FromSource event -> withEditor $ \_ editor -> H.liftEffect $ liftIO $ case event of
        Pointing line _ -> Browser.showSource editor line
        NotPointing -> Browser.showSource editor 0
        Picked line column -> Browser.gotoPosition editor line column

    withCode f = gets codeOf >>= traverse_ f

    request code source = CompileRequest {lesson = source, main = code.main, args = code.args, layers = code.layers}

    -- The playground's layers and extra arguments, kept for the next visit
    -- as the code is (and, as the code, for this one only when the browser
    -- denies its storage).
    keepSettings = do
      settings <- gets (.playground)
      void $ H.liftEffect $ tryAny $ do
        writeStorageItem LocalStorage Playground.layersKey (Playground.showLayers settings.layers)
        writeStorageItem LocalStorage Playground.argsKey settings.extra

    -- What the playground opens with ('Playground.opening'): what a lesson
    -- has just handed it (taken, so that coming back finds what the reader
    -- made of it since), else what was kept; its layers and arguments go
    -- into the state, and its code, if not the template, is given back for
    -- the editor. A hand-off is kept as the reader's at once, for a reload
    -- to find, as far as the browser lets it: what the editor opens with is
    -- the hand-off itself, not what the storage has after that write.
    openPlayground = do
      handed <- gets (.handoff)
      modify $ \s -> s {handoff = Nothing}
      kept <- Playground.Kept <$> stored Playground.codeKey <*> stored Playground.layersKey <*> stored Playground.argsKey
      let opened = Playground.opening handed kept
      modify $ \s -> s {playground = PlaygroundSettings {layers = opened.layers, extra = opened.extra}}
      for_ handed $ \handoff -> do
        keepSettings
        void $ H.liftEffect $ tryAny $ writeStorageItem LocalStorage Playground.codeKey handoff.code
      pure opened.code

    -- What @localStorage@ has under this key, if the browser lets it be
    -- read.
    stored key = join . rightToMaybe <$> H.liftEffect (tryAny (readStorageItem LocalStorage key))

    markDiagnostics editor diagnostics =
      H.liftEffect $ liftIO $ Browser.showDiagnostics editor (filter ((== "Lesson.hs") . (.file)) diagnostics)

    -- The types a run reported, if the code typechecked; else the last
    -- ones stay, as no longer the code's.
    keepTypes editor typechecked types
      | typechecked = do
          H.liftEffect $ liftIO $ Browser.showTypes editor types
          modify $ \s -> s {bindings = Just Bindings {list = types, current = True}}
      | otherwise = modify $ \s -> s {bindings = (\b -> b {current = False}) <$> s.bindings}

    -- A check once the reader has stopped typing for 'checkDelay': the
    -- change before this one waits no longer. Only once the compiler is
    -- there; the first check after it has loaded is 'startCheck''s.
    checkSoon = do
      st <- get
      for_ st.checkDue H.kill
      modify $ \s -> s {edits = s.edits + 1, checkDue = Nothing}
      when (isLoaded st.compiler) $ do
        due <- H.fork $ do
          liftIO (threadDelay checkDelay)
          -- From here on it is not killed: a change meanwhile drops its
          -- result instead, so the session never has a run taken from it
          -- half-way.
          modify $ \s -> s {checkDue = Nothing}
          startCheck
        modify $ \s -> s {checkDue = Just due}

    -- Check the editor's code, if the compiler can be asked (it is there,
    -- with the lesson's layers, and no compile is under way), no check is
    -- under way or waiting for the reader, and no check or compile has
    -- started from this code already. Called again whenever one of those
    -- may have changed: the compiler and the layers loaded, a compile or a
    -- check over, another lesson entered.
    startCheck = do
      st <- get
      when (canCompile st && not st.checking && isNothing st.checkDue && st.coveredEdit /= Just st.edits) $
        for_ ((,) <$> codeOf st <*> st.editor) $
          \(code, (editor, _)) -> do
            source <- H.liftEffect (liftIO (Browser.editorText editor))
            modify $ \s -> s {checking = True, coveredEdit = Just st.edits}
            checked <- liftIO $ Browser.typecheck (request code source)
            modify $ \s -> s {checking = False}
            now <- get
            -- Not if the code has changed since, or a compile has started
            -- (whose result, later, is of the same code or newer), or the
            -- reader is at another lesson (which counts as a change).
            when (now.edits == st.edits && now.runs == st.runs) $ for_ checked $ \result -> do
              let diagnostics = parseDiagnostics result.stderr
              markDiagnostics editor diagnostics
              keepTypes editor result.ok result.types
              modify $ \s -> s {diagnostics, lastCheck = Just (result.ms, result.ok)}
            startCheck

    withEditor f = do
      st <- get
      for_ ((,) <$> codeOf st <*> st.editor) $ \(code, (editor, _)) -> f code editor

    -- The editor, in the page's element, with the reader's code if any was
    -- kept, else the lesson's start (the playground's template); for the
    -- playground, what it opens with, layers and arguments first. A
    -- hand-off is for the page entered next only.
    enterPage = do
      route <- gets (.route)
      opened <-
        if route == Playground
          then Just <$> openPlayground
          else Nothing <$ modify (\s -> s {handoff = Nothing})
      withCode $ \code -> do
        modify $ \s -> s {edits = s.edits + 1}
        st <- get
        saved <- maybe (stored code.key) pure opened
        let text = fromMaybe code.start saved
        H.getHTMLElementRef st.editorHost >>= traverse_ \host -> do
          events <- liftIO HS.create
          subscription <- H.subscribe (FromEditor <$> events.emitter)
          editor <- H.liftEffect $ liftIO $ Browser.createEditor host text (readOnly st) (HS.notify events.listener)
          modify $ \s -> s {editor = Just (editor, subscription), edited = text /= code.start, visit = s.visit + 1}

    -- The Source mode over the result, and the editor telling where its
    -- cursor is, once there is something in the result to show the source
    -- of.
    startSource = do
      st <- get
      when (canSource st) $ for_ st.editor $ \(editor, _) ->
        H.getHTMLElementRef st.resultHost >>= traverse_ \element -> do
          events <- liftIO HS.create
          subscription <- H.subscribe (FromSource <$> events.emitter)
          source <- H.liftEffect $ liftIO $ Browser.startSource element editor (HS.notify events.listener)
          modify $ \s -> s {source = Just (source, subscription)}
          H.liftEffect $ liftIO $ Browser.setSourceMode editor True

    stopSource = do
      st <- get
      for_ st.source $ \(source, subscription) -> do
        H.unsubscribe subscription
        H.liftEffect $ liftIO $ Browser.stopSource source
        for_ st.editor $ \(editor, _) -> H.liftEffect $ liftIO $ do
          Browser.setSourceMode editor False
          Browser.showSource editor 0
        modify $ \s -> s {source = Nothing}

    leavePage = do
      stopSource
      st <- get
      for_ st.editor $ \(editor, subscription) -> do
        H.unsubscribe subscription
        H.liftEffect (liftIO (Browser.destroyEditor editor))
      for_ st.checkDue H.kill
      modify $ \s -> s {editor = Nothing, checkDue = Nothing}
      -- A compile under way will mount what it compiled into this lesson's
      -- element, or the next one's: ending the run is queued after it.
      when (st.mounted || isCompiling st.run) $ void $ H.fork (liftIO Browser.endRun)

    loadCompiler = do
      st <- get
      case st.compiler of
        Loading _ -> pass
        Loaded _ -> loadLayers
        _ -> do
          modify $ \s -> s {compiler = Loading ReadingManifest}
          progress <- liftIO HS.create
          subscription <- H.subscribe (Progressed <$> progress.emitter)
          root <- liftIO Browser.rootfsAddress
          let wanted = maybe [] (.layers) (codeOf st)
          liftIO (Browser.startCompiler root wanted (HS.notify progress.listener)) >>= \case
            Left message -> do
              -- The page tells nothing more through this start's progress:
              -- a Retry subscribes anew.
              H.unsubscribe subscription
              modify $ \s -> s {compiler = Broken message}
            Right () -> do
              -- A Retry after a layer failed finds the compiler started: the
              -- start reports nothing new then, so it reads as loaded here.
              modify $ \s ->
                s
                  { compiler = case s.compiler of
                      Loading _ -> Loaded s.knownMb
                      other -> other
                  }
              -- The layers wanted were fetched alongside, but may have
              -- failed: loadLayers asks, and says so for the lesson the
              -- reader is at now (who may have gone to one with other layers
              -- while it loaded).
              loadLayers
              syncEditor
              -- The lesson's code, as it is, without waiting for a change.
              startCheck
              -- After a Retry, the sizes may not have been read before.
              gets (.sizes) >>= \case
                Nothing -> readSizes
                Just _ -> pass

    readSizes = do
      root <- liftIO Browser.rootfsAddress
      liftIO (Browser.describe root) >>= \case
        Right sizes -> modify $ \s -> s {sizes = Just sizes}
        Left _ -> pass

    -- The current lesson's layers (the playground's checked ones), once the
    -- compiler has started. One load at a time: while one is under way the
    -- compiler reads as loading, so a lesson the reader arrives at meanwhile
    -- is not asked for; when the load ends, the lesson current then is
    -- checked again, so its layers follow and Compile is enabled there too
    -- (and a layer checked meanwhile in the playground is loaded). A layer
    -- that does not load is this lesson's failure, not the compiler's.
    loadLayers = do
      st <- get
      let wanted = maybe [] (.layers) (codeOf st)
      unless (st.loadingLayers || all (`elem` st.loadedLayers) wanted) $ do
        modify $ \s -> s {loadingLayers = True, layerFailure = Nothing}
        result <- liftIO (Browser.ensureLayers wanted)
        modify $ \s -> s {loadingLayers = False}
        now <- gets (.route)
        case result of
          Left message
            | now == st.route -> modify $ \s -> s {layerFailure = Just message}
            -- The reader has gone on: the lesson current now gets its own.
            | otherwise -> loadLayers
          Right () -> do
            modify $ \s -> s {loadedLayers = ordNub (s.loadedLayers <> wanted)}
            syncEditor
            loadLayers
            startCheck

    -- Whether the editor takes input follows the compiler: on a narrow
    -- screen it is read-only until the compiler has loaded, and an editor
    -- made while a layer was loading (the compiler reads as loading then)
    -- is unlocked once that is over.
    syncEditor = do
      st <- get
      for_ st.editor $ \(editor, _) -> H.liftEffect (liftIO (Browser.setEditorReadOnly editor (readOnly st)))

    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div
        [HP.class_ (HH.ClassName "learn")]
        [ header st
        , HH.withKeys
            HH.main
            [HP.class_ (HH.ClassName "learn-main")]
            [ case (lessonOf st.route, codeOf st) of
                (Just lesson, Just code) -> (lesson.slug, lessonPage st lesson code)
                (Nothing, Just code) -> ("playground", playgroundPage st code)
                _ -> ("", home st)
            ]
        ]

    link st target props = Router.link st.router Followed target props

    header st =
      HH.header
        [HP.class_ (HH.ClassName "learn-header")]
        [ link st Home [HP.class_ (HH.ClassName "learn-brand")] [HH.text "Learn Halogen"]
        , HH.nav
            [HP.class_ (HH.ClassName "learn-pager")]
            ( case lessonOf st.route of
                Nothing
                  | st.route == Playground -> [HH.span [HP.class_ (HH.ClassName "learn-position")] [HH.text "Playground"]]
                  | otherwise -> []
                Just lesson ->
                  [ HH.span [HP.class_ (HH.ClassName "learn-position")] [HH.text ("Lesson " <> show lesson.number <> " of " <> show (length lessons))]
                  , pager st "Previous" (neighbour (-1) lesson) "learn-prev"
                  , pager st "Next" (neighbour 1 lesson) "learn-next"
                  ]
            )
        , HH.span [HP.class_ (HH.ClassName "learn-status")] [HH.text (statusLine st)]
        ]

    pager st label target cls = case target of
      Just lesson -> link st (LessonPage lesson.slug) [HP.class_ (HH.ClassName cls), HP.title lesson.title] [HH.text label]
      Nothing -> HH.span [HP.class_ (HH.ClassName (cls <> " learn-disabled"))] [HH.text label]

    home st =
      HH.div
        [HP.class_ (HH.ClassName "learn-home")]
        [ HH.h1_ [HH.text "Learn Halogen"]
        , HH.p_
            [ HH.text
                "A tutorial for haskell-halogen, the Haskell port of PureScript's Halogen. Each lesson has a short text, code to change, and a Compile button: GHC itself runs in this page, compiled to WebAssembly, and compiles your code into a component that runs beside the lesson. Nothing is sent to a server."
            ]
        , HH.p_ [HH.text (downloadNote st)]
        , HH.p_
            [ HH.text "Just want to write code? The "
            , link st Playground [HP.class_ (HH.ClassName "learn-to-playground")] [HH.text "playground"]
            , HH.text " is the same compiler with no lesson around it: pick the layers your code needs."
            ]
        , HH.ol
            [HP.class_ (HH.ClassName "learn-lessons")]
            [ HH.li_
                ( link st (LessonPage lesson.slug) [] [HH.text lesson.title]
                    : [HH.span [HP.class_ (HH.ClassName "learn-note")] [HH.text (" (with " <> T.intercalate ", " lesson.layers <> layerSizes st lesson <> ")")] | not (null lesson.layers)]
                )
            | lesson <- lessons
            ]
        , HH.p_ [HH.text "Start with the first lesson; each builds on the one before."]
        ]

    lessonPage st lesson code =
      HH.div
        [HP.class_ (HH.ClassName "learn-lesson")]
        [ HH.article
            [HP.class_ (HH.ClassName "learn-text")]
            ( [HH.h1_ [HH.text (show lesson.number <> ". " <> lesson.title)]]
                <> View.blocks lesson.text
                <> [ HH.nav
                       [HP.class_ (HH.ClassName "learn-pager learn-pager-bottom")]
                       [ pager st "Previous lesson" (neighbour (-1) lesson) "learn-prev"
                       , link st Playground [HP.class_ (HH.ClassName "learn-to-playground")] [HH.text "Playground"]
                       , pager st "Next lesson" (neighbour 1 lesson) "learn-next"
                       ]
                   ]
            )
        , codeColumn st code
        , resultColumn st
        ]

    -- The playground: a narrow panel in place of a lesson's text, with the
    -- layers to pick and the arguments to add, beside the same code and
    -- result columns.
    playgroundPage st code =
      HH.div
        [HP.class_ (HH.ClassName "learn-lesson learn-playground")]
        [ HH.article
            [HP.class_ (HH.ClassName "learn-text learn-playground-panel")]
            [ HH.h1_ [HH.text "Playground"]
            , HH.p_
                [ HH.text
                    "The lessons' compiler with no lesson around it: GHC compiles your module Lesson in this page, and nothing is sent anywhere. Pick the layers your code needs, and the page passes GHC their packages."
                ]
            , HH.fieldset
                [HP.class_ (HH.ClassName "learn-layers")]
                ( HH.legend_ [HH.text "Layers"]
                    : case st.sizes of
                      Just sizes -> [HH.ul_ (map (layerChoice st) sizes.layers)]
                      Nothing -> [HH.p [HP.class_ (HH.ClassName "learn-note")] [HH.text "The layers are listed once the page has read what the compiler is made of."]]
                )
            , HH.p
                [HP.class_ (HH.ClassName "learn-extra")]
                [ HH.label [HP.for "learn-extra-args"] [HH.text "Extra GHC arguments"]
                , HH.input
                    [ HP.id "learn-extra-args"
                    , HP.class_ (HH.ClassName "learn-extra-args")
                    , HP.type_ I.InputText
                    , HP.value st.playground.extra
                    , HP.placeholder "-fdefer-typed-holes"
                    , HP.spellcheck False
                    , HE.onValueInput SetExtraArgs
                    ]
                ]
            , HH.p [HP.class_ (HH.ClassName "learn-note")] [HH.text "Your code, the layers and the arguments are kept in this browser for your next visit. Reset puts the example back."]
            , HH.nav
                [HP.class_ (HH.ClassName "learn-pager learn-pager-bottom")]
                [link st Home [HP.class_ (HH.ClassName "learn-to-lessons")] [HH.text "Lessons"]]
            ]
        , codeColumn st code
        , resultColumn st
        ]

    -- A layer to check: its name, its packages, and what it downloads, or
    -- that it is loaded.
    layerChoice st layer =
      let checked = layer.name `elem` st.playground.layers
          sizeNote
            | checked && layer.name `elem` st.loadedLayers = "loaded"
            | otherwise = maybe "" (\bytes -> megabytes bytes <> " MB") layer.size
       in HH.li_
            [ HH.label
                [HP.class_ (HH.ClassName "learn-layer")]
                [ HH.input [HP.type_ I.InputCheckbox, HP.value layer.name, HP.checked checked, HE.onChecked (ToggleLayer layer.name)]
                , HH.text (" " <> layer.name <> " ")
                , HH.span [HP.class_ (HH.ClassName "learn-note")] [HH.text (T.intercalate ", " layer.packages)]
                , HH.span [HP.class_ (HH.ClassName "learn-note learn-layer-note")] [HH.text (if T.null sizeNote then "" else " · " <> sizeNote)]
                ]
            ]

    codeColumn st code =
      HH.withKeys
        HH.section
        [HP.class_ (HH.ClassName "learn-code")]
        ( [
            ( "toolbar"
            , HH.div
                [HP.class_ (HH.ClassName "learn-toolbar")]
                ( [ HH.button
                      [ HP.class_ (HH.ClassName "learn-compile")
                      , HP.disabled (not (canCompile st))
                      , HE.onClick (const Compile)
                      , HP.title "Ctrl+Enter (Cmd+Enter on a Mac) in the editor"
                      ]
                      [HH.text (if isCompiling st.run then "Compiling…" else "Compile")]
                  , HH.button [HP.class_ (HH.ClassName "learn-reset"), HP.disabled (isNothing st.editor), HE.onClick (const Reset)] [HH.text "Reset"]
                  ]
                    -- A lesson's: the playground has no solution, and is
                    -- where its code would be opened.
                    <> concat
                      [ [ HH.button [HP.class_ (HH.ClassName "learn-solution"), HP.disabled (isNothing st.editor), HE.onClick (const ShowSolution)] [HH.text "Solution"]
                        , HH.button
                            [ HP.class_ (HH.ClassName "learn-open-playground")
                            , HP.disabled (isNothing st.editor)
                            , HP.title "Your code, in the playground, with this lesson's layers"
                            , HE.onClick (const OpenInPlayground)
                            ]
                            [HH.text "Open in playground"]
                        ]
                      | isJust code.solution
                      ]
                    <> [HH.span [HP.class_ (HH.ClassName "learn-file")] [HH.text ("Lesson.hs" <> if st.edited then ", edited" else "")]]
                )
            )
          , -- The editor's own element comes first, and stays: the code
            -- shown until the editor is there goes after it.
            ("editor", HH.div [HP.class_ (HH.ClassName "learn-editor"), HP.ref st.editorHost] [])
          ]
            <> [("fallback", HH.pre [HP.class_ (HH.ClassName "learn-fallback")] [HH.text code.start]) | isNothing st.editor]
            <> [("check", HH.p [HP.class_ (HH.ClassName "learn-check")] [HH.text text]) | Just text <- [checkLine st]]
            <> [("diagnostics", diagnosticList st.diagnostics) | not (null st.diagnostics)]
            <> [("bindings", bindingList b) | Just b <- [st.bindings], not (null b.list)]
        )

    -- What the checks are doing, once there has been one; nothing of a
    -- compile, whose outcome is the result's.
    checkLine st
      | st.checking = Just "Checking…"
      | otherwise = case st.lastCheck of
          Just (ms, True) -> Just ("Checked in " <> seconds ms <> ": it compiles.")
          Just (ms, False) -> Just ("Checked in " <> seconds ms <> ": it does not compile.")
          Nothing -> Nothing

    bindingList b =
      HH.div
        [HP.class_ (HH.ClassName "learn-bindings")]
        ( [HH.h2_ [HH.text "Top-level types"]]
            <> [HH.p [HP.class_ (HH.ClassName "learn-note")] [HH.text "From the last time the code compiled."] | not b.current]
            <> [ HH.ul_
                   [ HH.li_
                       [ HH.button
                           [ HP.class_ (HH.ClassName "learn-binding")
                           , HP.title ("Line " <> show binding.line)
                           , HE.onClick (const (Goto binding.line binding.column))
                           ]
                           [HH.code_ [HH.text (displayName binding <> " :: " <> binding.type_)]]
                       ]
                   | binding <- b.list
                   ]
               ]
        )

    diagnosticList ds =
      HH.ul
        [HP.class_ (HH.ClassName "learn-diagnostics")]
        [ HH.li
            [HP.class_ (HH.ClassName (case d.severity of Error -> "learn-error"; Warning -> "learn-warning"))]
            [ if d.file == "Lesson.hs"
                then HH.button [HP.class_ (HH.ClassName "learn-location"), HE.onClick (const (Goto d.line d.column))] [HH.text (location d)]
                else HH.span [HP.class_ (HH.ClassName "learn-location")] [HH.text (location d)]
            , HH.pre_ [HH.text d.message]
            ]
        | d <- ds
        ]

    location d = d.file <> ":" <> show d.line <> ":" <> show d.column <> " " <> (case d.severity of Error -> "error"; Warning -> "warning")

    resultColumn st =
      HH.withKeys
        HH.section
        [HP.class_ (HH.ClassName "learn-result-column")]
        ( [
            ( "toolbar"
            , HH.div
                [HP.class_ (HH.ClassName "learn-toolbar")]
                [ HH.button
                    [ HP.class_ (HH.ClassName "learn-source")
                    , HP.disabled (not (canSource st))
                    , HP.attr (H.AttrName "aria-pressed") (if isJust st.source then "true" else "false")
                    , HP.title "Point at an element of the result to see the line of code that made it"
                    , HE.onClick (const ToggleSource)
                    ]
                    [HH.text "Source"]
                ]
            )
          ]
            <> [ ( "source"
                 , HH.p
                     [HP.class_ (HH.ClassName "learn-note")]
                     [HH.text "Point at an element to see the line that made it; click it to go there (the program does not get the click). The editor's cursor outlines the element its line made."]
                 )
               | isJust st.source
               ]
            <> [("compiler", compilerPanel st) | not (isLoaded st.compiler)]
            <> [("layers", layersPanel st message) | isLoaded st.compiler, Just message <- [st.layerFailure]]
            <> [ ("outcome", HH.p [HP.class_ (HH.ClassName "learn-outcome"), HP.attr (H.AttrName "data-run") (show st.runs)] [HH.text (outcome st)])
               , -- Never patched inside: the compiled program's.
                 ("result", HH.div [HP.id "learn-result", HP.class_ (HH.ClassName "learn-result"), HP.ref st.resultHost] [])
               ]
            <> output st.run
        )

    output = \case
      Ran result ->
        [("stdout", HH.pre [HP.class_ (HH.ClassName "learn-stdout")] [HH.text result.stdout]) | not (T.null result.stdout)]
          <> [ ( "stderr"
               , HH.details
                   [HP.class_ (HH.ClassName "learn-stderr")]
                   [HH.summary_ [HH.text "What GHC said"], HH.pre_ [HH.text result.stderr]]
               )
             | not (T.null result.stderr)
             ]
      _ -> []

    compilerPanel st =
      HH.div
        [HP.class_ (HH.ClassName "learn-compiler")]
        ( case st.compiler of
            NotLoaded
              | st.narrow ->
                  [ HH.p_ [HH.text ("On a small screen the compiler is not loaded by itself: " <> sizeOf st <> " to download, and over a gigabyte of memory once it runs, more than most phones give a page. The lesson and its code are all here to read.")]
                  , HH.button [HP.class_ (HH.ClassName "learn-load"), HE.onClick (const LoadCompiler)] [HH.text "Load the compiler anyway"]
                  ]
              | otherwise -> [HH.p_ [HH.text "The compiler loads once the page has started."]]
            Loading p -> progressView st p
            Loaded _ -> []
            Broken message ->
              [ HH.p [HP.class_ (HH.ClassName "learn-error")] [HH.text ("The compiler did not load: " <> message)]
              , HH.button [HP.class_ (HH.ClassName "learn-retry"), HE.onClick (const LoadCompiler)] [HH.text "Retry"]
              ]
        )

    layersPanel st message =
      HH.div
        [HP.class_ (HH.ClassName "learn-compiler")]
        [ HH.p [HP.class_ (HH.ClassName "learn-error")] [HH.text (capitalised (libraries st) <> " did not load: " <> message)]
        , HH.button [HP.class_ (HH.ClassName "learn-retry-layers"), HE.onClick (const LoadCompiler)] [HH.text "Retry"]
        ]

    -- What a layer load is for, in the words of the page it is on.
    libraries st = if st.route == Playground then "the layers you checked" else "the lesson's libraries"
    capitalised t = T.toUpper (T.take 1 t) <> T.drop 1 t

    progressView st = \case
      Downloading loaded total _ ->
        [ HH.p_ [HH.text ("Downloading GHC: " <> megabytes loaded <> maybe " MB" (\t -> " of " <> megabytes t <> " MB") total)]
        , bar (fmap (\t -> fromIntegral loaded / fromIntegral (max 1 t)) total)
        ]
      ReadingManifest -> [HH.p_ [HH.text "Looking for the compiler…"], bar Nothing]
      ReadingCache -> [HH.p_ [HH.text "Reading GHC from your browser's cache…"], bar Nothing]
      Extracting -> [HH.p_ [HH.text "Extracting…"], bar Nothing]
      Starting -> [HH.p_ [HH.text "Starting GHC…"], bar Nothing]
      LoadingLayers -> [HH.p_ [HH.text ("Loading " <> libraries st <> "…")], bar Nothing]
      Ready _ -> []
      LoadFailed message -> [HH.p_ [HH.text message]]

    -- A fraction, or Nothing for a bar that only says something is going on.
    bar :: Maybe Double -> HH.HTML w Action
    bar fraction =
      HH.div
        [HP.class_ (HH.ClassName ("learn-bar" <> maybe " learn-bar-busy" (const "") fraction))]
        [HH.div [HP.class_ (HH.ClassName "learn-bar-fill"), HP.attr (H.AttrName "style") ("width: " <> maybe "30" (\f -> show (round (f * 100) :: Int)) fraction <> "%")] []]

    outcome st = case st.run of
      NotRun
        | isLoaded st.compiler -> "Press Compile to run your code here."
        | otherwise -> "Your code's component will show here."
      Compiling -> "Compiling…"
      Unrun message -> "The compiler could not run: " <> message
      Ran result
        | result.ok -> "Compiled and ran in " <> seconds result.ms <> "."
        | result.compiled -> "It compiled, and then stopped: " <> fromMaybe "" result.failure
        | st.mounted -> "It did not compile; what is below is from the last time it did."
        | otherwise -> "It did not compile."

    statusLine st = case st.compiler of
      Loaded mb -> ghcName st <> " in your browser, " <> megabytes' mb <> " MB loaded"
      Loading (Downloading _ _ _) -> "Downloading GHC…"
      Loading _ -> "Starting GHC…"
      Broken _ -> "GHC did not load"
      NotLoaded
        | st.narrow -> "GHC is not loaded"
        | otherwise -> "GHC loads in the background"

    ghcName st = case st.sizes of
      Just s
        | Just version <- T.stripPrefix "ghc-" s.ghc -> "GHC " <> T.intercalate "." (take 2 (T.splitOn "." version))
      _ -> "GHC"

    downloadNote st =
      "The compiler is "
        <> sizeOf st
        <> " to download, once: your browser keeps it for the next visit. Running, it takes over a gigabyte of memory, so on a screen narrower than 900 pixels it loads only when you ask."

    sizeOf st = case (.base) =<< st.sizes of
      Just bytes -> megabytes bytes <> " MB"
      Nothing -> "tens of megabytes"

    layerSizes st lesson = case st.sizes of
      Just s
        | Just bytes <- sum <$> traverse (\name -> join (lookup name [(l.name, l.size) | l <- s.layers])) lesson.layers ->
            ", " <> megabytes' (fromIntegral bytes / 1e6 :: Double) <> " MB more"
      _ -> ""

-- | How long the reader stops typing before the code is checked, in
-- microseconds: long enough not to check every word, as the page's thread is
-- the compiler's while it checks.
checkDelay :: Int
checkDelay = 1200000

-- | Lessons are numbered from 1, in the order of 'lessons'.
neighbour :: Int -> Lesson -> Maybe Lesson
neighbour step lesson = head (drop (lesson.number - 1 + step) lessons) <* guard (lesson.number - 1 + step >= 0)

-- | The compiler is there, with the page's layers, and is not compiling.
-- The playground's layers' packages come from the manifest, so with a layer
-- checked it waits for that too.
canCompile :: State -> Bool
canCompile st =
  isLoaded st.compiler
    && isJust st.editor
    && not (isCompiling st.run)
    && all (`elem` st.loadedLayers) wanted
    && (null wanted || st.route /= Playground || isJust st.sizes)
  where
    wanted = maybe [] (.layers) (codeOf st)

-- | The Source mode needs the editor, and a compile's elements to read.
canSource :: State -> Bool
canSource st = st.mounted && isJust st.editor

readOnly :: State -> Bool
readOnly st = st.narrow && not (isLoaded st.compiler)

isLoaded :: Compiler -> Bool
isLoaded = \case
  Loaded _ -> True
  _ -> False

isCompiling :: Run -> Bool
isCompiling = \case
  Compiling -> True
  _ -> False

storageKey :: Lesson -> Text
storageKey lesson = "learn:" <> lesson.slug

megabytes :: Int -> Text
megabytes bytes = megabytes' (fromIntegral bytes / 1e6)

megabytes' :: Double -> Text
megabytes' mb = toS (showFFloat (Just (if mb < 1 then 2 else 1)) mb "")

seconds :: Int -> Text
seconds ms = toS (showFFloat (Just 1) (fromIntegral ms / 1000 :: Double) "") <> " s"
