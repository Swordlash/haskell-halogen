-- | The tutorial: an introduction with the list of lessons, and a page per
-- lesson with its text, a code editor and the result of compiling the code,
-- which GHC does in the page ("Learn.Browser").
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
-- What a compile mounts goes into @#learn-result@, an element this
-- component renders once per lesson (the lesson's page is keyed by its
-- slug) and never patches inside: its content is the compiled program's,
-- another Halogen tree, run by the compiler's runtime. Before the page takes
-- the element away (another route), that tree is ended ('endRun').
module Learn.App (component) where

import Control.Monad.UUID (MonadUUID)
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
import Learn.Browser (CompileRequest (..), Editor, EditorEvent (..), Progress (..), RunResult (..), Sizes (..))
import Learn.Browser qualified as Browser
import Learn.Diagnostics (Diagnostic (..), Severity (..), parseDiagnostics)
import Learn.Lesson (Lesson (..), wrapper)
import Learn.Lessons (lessons)
import Learn.Route (Route (..), lessonOf)
import Learn.View qualified as View
import Numeric (showFFloat)
import Protolude hiding (State, link)
import UnliftIO (MonadUnliftIO)

data State = State
  { router :: Router Route
  , route :: Route
  , editorHost :: H.RefLabel
  , editor :: Maybe (Editor, H.SubscriptionId)
  , narrow :: Bool
  , compiler :: Compiler
  , sizes :: Maybe Sizes
  , loadedLayers :: [Text]
  , loadingLayers :: Bool
  , runs :: Int
  , visit :: Int
  , knownMb :: Double
  -- ^ How many megabytes the compiler reported loaded when it was last
  -- ready, for a Retry that finds it already started and gets no new report.
  -- ^ Counts the lessons entered: a compile started in one visit must not
  -- report into a later one, even of the same lesson, whose editor is new.
  -- ^ How many compiles have started, shown on the outcome as @data-run@
  -- so that a test can tell a new outcome from the last one.
  -- ^ A layer load is under way; the lesson's layers are checked again
  -- when it ends, in case the reader has moved to another lesson since.
  , edited :: Bool
  -- ^ The editor's text is not the lesson's @Start.hs@.
  , run :: Run
  , mounted :: Bool
  -- ^ A compile has mounted something into this lesson's @#learn-result@.
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
  | Ran RunResult [Diagnostic]
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

component :: forall q o m. (MonadBrowserDOM m, MonadUnliftIO m, MonadUUID m) => H.Component q (Router Route) o m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize, H.finalize = Just Finalize}
      }
  where
    initialState router = do
      route <- Router.current router
      editorHost <- H.newRefLabel "editor"
      pure
        State
          { router
          , route
          , editorHost
          , editor = Nothing
          , narrow = False
          , compiler = NotLoaded
          , sizes = Nothing
          , loadedLayers = []
          , loadingLayers = False
          , runs = 0
          , visit = 0
          , knownMb = 0
          , edited = False
          , run = NotRun
          , mounted = False
          }

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Initialize -> when inBrowser $ do
        router <- gets (.router)
        void $ H.subscribe (Arrived <$> Router.changes router)
        narrow <- H.liftEffect (liftIO Browser.narrowScreen)
        modify $ \s -> s {narrow}
        enterLesson
        -- What the compiler weighs, for the page to say, whether or not it
        -- is loaded: the manifest is small.
        void $ H.fork readSizes
        unless narrow loadCompiler
      Finalize -> leaveLesson
      Arrived r -> do
        current <- gets (.route)
        unless (r == current) $ do
          leaveLesson
          modify $ \s -> s {route = r, edited = False, run = NotRun, mounted = False}
          H.liftEffect (liftIO Browser.scrollToTop)
          enterLesson
          gets (.compiler) >>= \case
            Loaded _ -> loadLayers
            _ -> pass
      Followed f -> do
        router <- gets (.router)
        Router.follow router f
      Progressed p -> do
        modify $ \s -> case p of
          Ready mb -> s {compiler = Loaded mb, knownMb = mb}
          LoadFailed message -> s {compiler = Broken message}
          _ -> s {compiler = Loading p}
        syncEditor
      LoadCompiler -> loadCompiler
      FromEditor (Changed text) -> withLesson $ \lesson -> do
        -- What is kept is the reader's own code: none, once it is the
        -- lesson's start again (Reset).
        H.liftEffect $
          if text == lesson.start
            then removeStorageItem LocalStorage (storageKey lesson)
            else writeStorageItem LocalStorage (storageKey lesson) text
        let edited = text /= lesson.start
        was <- gets (.edited)
        when (was /= edited) $ modify $ \s -> s {edited}
      FromEditor CompileRequested -> handleAction Compile
      Compile -> do
        st <- get
        when (canCompile st) $ for_ ((,) <$> lessonOf st.route <*> st.editor) $ \(lesson, (editor, _)) -> do
          source <- H.liftEffect (liftIO (Browser.editorText editor))
          modify $ \s -> s {run = Compiling, runs = s.runs + 1}
          compiled <-
            liftIO $
              Browser.compile
                CompileRequest {lesson = source, main = wrapper lesson, args = lesson.args, layers = lesson.layers}
          -- The reader may have left meanwhile, even for this same lesson
          -- again: that visit's editor is another one, and its run its own.
          now <- gets (.visit)
          when (now == st.visit) $ case compiled of
            Left message -> modify $ \s -> s {run = Unrun message}
            Right result -> do
              let diagnostics = parseDiagnostics result.stderr
              H.liftEffect $ liftIO $ Browser.showDiagnostics editor (filter ((== "Lesson.hs") . (.file)) diagnostics)
              modify $ \s -> s {run = Ran result diagnostics, mounted = s.mounted || result.compiled}
      Reset -> withEditor $ \lesson editor -> H.liftEffect (liftIO (Browser.setEditorText editor lesson.start))
      ShowSolution -> withEditor $ \lesson editor -> do
        edited <- gets (.edited)
        sure <-
          if edited
            then H.liftEffect (liftIO (Browser.confirm "Replace your code with the lesson's solution? Your code is not kept."))
            else pure True
        when sure $ H.liftEffect (liftIO (Browser.setEditorText editor lesson.solution))
      Goto line column -> withEditor $ \_ editor -> H.liftEffect (liftIO (Browser.gotoPosition editor line column))

    withLesson f = gets (lessonOf . (.route)) >>= traverse_ f

    withEditor f = do
      st <- get
      for_ ((,) <$> lessonOf st.route <*> st.editor) $ \(lesson, (editor, _)) -> f lesson editor

    -- The editor, in the lesson's element, with the reader's code if any
    -- was kept, else the lesson's start.
    enterLesson = withLesson $ \lesson -> do
      st <- get
      saved <- H.liftEffect (readStorageItem LocalStorage (storageKey lesson))
      let text = fromMaybe lesson.start saved
      H.getHTMLElementRef st.editorHost >>= traverse_ \host -> do
        events <- liftIO HS.create
        subscription <- H.subscribe (FromEditor <$> events.emitter)
        editor <- H.liftEffect $ liftIO $ Browser.createEditor host text (readOnly st) (HS.notify events.listener)
        modify $ \s -> s {editor = Just (editor, subscription), edited = text /= lesson.start, visit = s.visit + 1}

    leaveLesson = do
      st <- get
      for_ st.editor $ \(editor, subscription) -> do
        H.unsubscribe subscription
        H.liftEffect (liftIO (Browser.destroyEditor editor))
      modify $ \s -> s {editor = Nothing}
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
          void $ H.subscribe (Progressed <$> progress.emitter)
          root <- liftIO Browser.rootfsAddress
          let wanted = maybe [] (.layers) (lessonOf st.route)
          liftIO (Browser.startCompiler root wanted (HS.notify progress.listener)) >>= \case
            Left message -> modify $ \s -> s {compiler = Broken message}
            Right () -> do
              -- A Retry after a layer failed finds the compiler started: the
              -- start reports nothing new then, so it reads as loaded here.
              modify $ \s ->
                s
                  { loadedLayers = ordNub (s.loadedLayers <> wanted)
                  , compiler = case s.compiler of
                      Loading _ -> Loaded s.knownMb
                      other -> other
                  }
              -- The reader may have gone to a lesson with other layers
              -- while it loaded.
              loadLayers
              syncEditor
              -- After a Retry, the sizes may not have been read before.
              gets (.sizes) >>= \case
                Nothing -> readSizes
                Just _ -> pass

    readSizes = do
      root <- liftIO Browser.rootfsAddress
      liftIO (Browser.describe root) >>= \case
        Right sizes -> modify $ \s -> s {sizes = Just sizes}
        Left _ -> pass

    -- The current lesson's layers, once the compiler has started. One load
    -- at a time: while one is under way the compiler reads as loading, so
    -- a lesson the reader arrives at meanwhile is not asked for; when the
    -- load ends, the lesson current then is checked again, so its layers
    -- follow and Compile is enabled there too.
    loadLayers = do
      st <- get
      let wanted = maybe [] (.layers) (lessonOf st.route)
      unless (st.loadingLayers || all (`elem` st.loadedLayers) wanted) $ do
        modify $ \s -> s {loadingLayers = True}
        result <- liftIO (Browser.ensureLayers wanted)
        modify $ \s -> s {loadingLayers = False}
        case result of
          Left message -> modify $ \s -> s {compiler = Broken message}
          Right () -> do
            modify $ \s -> s {loadedLayers = ordNub (s.loadedLayers <> wanted)}
            syncEditor
            loadLayers

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
            [ case lessonOf st.route of
                Nothing -> ("", home st)
                Just lesson -> (lesson.slug, lessonPage st lesson)
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
                Nothing -> []
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

    lessonPage st lesson =
      HH.div
        [HP.class_ (HH.ClassName "learn-lesson")]
        [ HH.article
            [HP.class_ (HH.ClassName "learn-text")]
            ( [HH.h1_ [HH.text (show lesson.number <> ". " <> lesson.title)]]
                <> View.blocks lesson.text
                <> [ HH.nav
                       [HP.class_ (HH.ClassName "learn-pager learn-pager-bottom")]
                       [pager st "Previous lesson" (neighbour (-1) lesson) "learn-prev", pager st "Next lesson" (neighbour 1 lesson) "learn-next"]
                   ]
            )
        , codeColumn st lesson
        , resultColumn st
        ]

    codeColumn st lesson =
      HH.withKeys
        HH.section
        [HP.class_ (HH.ClassName "learn-code")]
        ( [
            ( "toolbar"
            , HH.div
                [HP.class_ (HH.ClassName "learn-toolbar")]
                [ HH.button
                    [ HP.class_ (HH.ClassName "learn-compile")
                    , HP.disabled (not (canCompile st))
                    , HE.onClick (const Compile)
                    , HP.title "Ctrl+Enter (Cmd+Enter on a Mac) in the editor"
                    ]
                    [HH.text (if isCompiling st.run then "Compiling…" else "Compile")]
                , HH.button [HP.class_ (HH.ClassName "learn-reset"), HP.disabled (isNothing st.editor), HE.onClick (const Reset)] [HH.text "Reset"]
                , HH.button [HP.class_ (HH.ClassName "learn-solution"), HP.disabled (isNothing st.editor), HE.onClick (const ShowSolution)] [HH.text "Solution"]
                , HH.span [HP.class_ (HH.ClassName "learn-file")] [HH.text ("Lesson.hs" <> if st.edited then ", edited" else "")]
                ]
            )
          , -- The editor's own element comes first, and stays: the code
            -- shown until the editor is there goes after it.
            ("editor", HH.div [HP.class_ (HH.ClassName "learn-editor"), HP.ref st.editorHost] [])
          ]
            <> [("fallback", HH.pre [HP.class_ (HH.ClassName "learn-fallback")] [HH.text lesson.start]) | isNothing st.editor]
            <> [("diagnostics", diagnosticList diagnostics) | not (null diagnostics)]
        )
      where
        diagnostics = case st.run of
          Ran _ ds -> ds
          _ -> []

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
        ( [("compiler", compilerPanel st) | not (isLoaded st.compiler)]
            <> [ ("outcome", HH.p [HP.class_ (HH.ClassName "learn-outcome"), HP.attr (H.AttrName "data-run") (show st.runs)] [HH.text (outcome st)])
               , -- Never patched inside: the compiled program's.
                 ("result", HH.div [HP.id "learn-result", HP.class_ (HH.ClassName "learn-result")] [])
               ]
            <> output st.run
        )

    output = \case
      Ran result _ ->
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
            Loading p -> progressView p
            Loaded _ -> []
            Broken message ->
              [ HH.p [HP.class_ (HH.ClassName "learn-error")] [HH.text ("The compiler did not load: " <> message)]
              , HH.button [HP.class_ (HH.ClassName "learn-retry"), HE.onClick (const LoadCompiler)] [HH.text "Retry"]
              ]
        )

    progressView = \case
      Downloading loaded total _ ->
        [ HH.p_ [HH.text ("Downloading GHC: " <> megabytes loaded <> maybe " MB" (\t -> " of " <> megabytes t <> " MB") total)]
        , bar (fmap (\t -> fromIntegral loaded / fromIntegral (max 1 t)) total)
        ]
      ReadingManifest -> [HH.p_ [HH.text "Looking for the compiler…"], bar Nothing]
      Extracting -> [HH.p_ [HH.text "Extracting…"], bar Nothing]
      Starting -> [HH.p_ [HH.text "Starting GHC…"], bar Nothing]
      LoadingLayers -> [HH.p_ [HH.text "Loading the lesson's libraries…"], bar Nothing]
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
      Ran result _
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
        | Just bytes <- sum <$> traverse (\name -> join (lookup name s.layers)) lesson.layers -> ", " <> megabytes' (fromIntegral bytes / 1e6 :: Double) <> " MB more"
      _ -> ""

-- | Lessons are numbered from 1, in the order of 'lessons'.
neighbour :: Int -> Lesson -> Maybe Lesson
neighbour step lesson = head (drop (lesson.number - 1 + step) lessons) <* guard (lesson.number - 1 + step >= 0)

canCompile :: State -> Bool
canCompile st =
  isLoaded st.compiler
    && isJust st.editor
    && not (isCompiling st.run)
    && all (`elem` st.loadedLayers) (maybe [] (.layers) (lessonOf st.route))

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
