{-# LANGUAGE TemplateHaskell #-}

-- | A page whose islands take a while to start, under a shell that routes.
--
-- The shell shows the page of the route it is at: a plain one, or a slow
-- one whose page view holds an editor, which holds two islands. An island
-- starts by waiting, in its @Initialize@, for something that takes seconds
-- (as one rendering a diagram waits for the library that draws it), and
-- shows @waiting@ until it has, then @loaded@.
--
-- While the islands wait, everything around them goes on: an
-- @Initialize@ that waits holds up nothing but itself. The editor's
-- outputs reach the page view, and through it the shell; the page view's
-- queries to the editor are answered; the shell, whose @Initialize@
-- subscribes to the router's changes, follows a link clicked on the page
-- (by the page view's own router link, or by the editor asking the page view
-- to leave), and the page it goes to replaces the slow one, islands and
-- all, at once.
module Example.Islands
  ( Route
  , Input (..)
  , component
  )
where

import Data.Map.Strict qualified as M
import Data.Row (Empty, type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (Safety (..), browserJS)
import Halogen.JSBits.Promise (Promise (..), await)
import Halogen.Router qualified as Router
import Protolude hiding (State)

-- What an island waits for is test/jsbits/islands.js's: a promise that
-- settles after so many milliseconds. Natively 'await' returns at once,
-- which is never seen, since the suite only reports a skip there.
$( browserJS
     ["test/jsbits/islands.js"]
     [("js_islands_wait", "halogen_islands_wait", Unsafe, [t|Int -> IO Promise|])]
 )

-- | The routes: the plain page, or the slow page of this number.
type Route = Maybe Int

-- | The shell's router, and how long an island waits when it starts.
data Input = Input
  { router :: Router.Router Route
  , waitMs :: Int
  }

----------------------------------------------------------------------
-- The shell

data State = State
  { input :: Input
  , route :: Route
  , heard :: [Text]
  }

data Action
  = Initialize
  | Arrived Route
  | Followed (Router.Follow Route)
  | FromPage PageOutput

type Slots = ("page" .== H.Slot (Const Void) PageOutput Int)

component :: forall q o m. (MonadIO m) => H.Component q Input o m
component =
  H.component
    ( \input -> do
        route <- Router.current input.router
        pure State {input, route, heard = []}
    )
    render
    [H.onAction handleAction, H.onInitialize (handleAction Initialize)]
  where
    render :: State -> H.ComponentHTML Action Slots m
    render st =
      HH.div_
        [ Router.link st.input.router Followed (Just 1) [HP.class_ (HH.ClassName "slow-link")] [HH.text "the slow page"]
        , HH.p [HP.class_ (HH.ClassName "heard")] [HH.text (T.unwords st.heard)]
        , case st.route of
            Nothing -> HH.p [HP.class_ (HH.ClassName "plain")] [HH.text "the plain page"]
            Just n -> HH.slot "page" n pageView (st.input, n) FromPage
        ]

    handleAction :: Action -> H.HalogenM State Action Slots o m ()
    handleAction = \case
      -- Subscribed here, when the shell starts: a link followed before the
      -- shell's own Initialize has run changes the address, and nobody
      -- hears of it.
      Initialize -> do
        router <- gets (.input.router)
        void (H.subscribe (Arrived <$> Router.changes router))
      Arrived route -> modify $ \st -> st {route}
      Followed f -> gets (.input.router) >>= (`Router.follow` f)
      FromPage (Said t) -> modify $ \st -> st {heard = st.heard <> [t]}
      FromPage Leave -> do
        router <- gets (.input.router)
        H.liftEffect (Router.navigate router Nothing)

----------------------------------------------------------------------
-- The slow page's view

data PageOutput = Said Text | Leave

data PageState = PageState
  { input :: Input
  , number :: Int
  , said :: [Text]
  , answer :: Text
  }

data PageAction
  = PageFollowed (Router.Follow Route)
  | Ask
  | FromEditor EditorOutput

type PageSlots = ("editor" .== H.Slot EditorQuery EditorOutput ())

pageView :: forall q m. (MonadIO m) => H.Component q (Input, Int) PageOutput m
pageView =
  H.component
    (\(input, number) -> pure PageState {input, number, said = [], answer = "not asked"})
    render
    [H.onAction handleAction]
  where
    render :: PageState -> H.ComponentHTML PageAction PageSlots m
    render st =
      HH.div
        [HP.class_ (HH.ClassName "page")]
        [ HH.p_ [HH.text ("the slow page " <> show st.number)]
        , Router.link st.input.router PageFollowed Nothing [HP.class_ (HH.ClassName "plain-link")] [HH.text "the plain page"]
        , HH.button [HP.class_ (HH.ClassName "ask"), HE.onClick (const Ask)] [HH.text "Is the editor dirty?"]
        , HH.p [HP.class_ (HH.ClassName "answer")] [HH.text st.answer]
        , HH.p [HP.class_ (HH.ClassName "page-heard")] [HH.text (T.unwords st.said)]
        , HH.slot "editor" () editor st.input.waitMs FromEditor
        ]

    handleAction :: PageAction -> H.HalogenM PageState PageAction PageSlots PageOutput m ()
    handleAction = \case
      PageFollowed f -> gets (.input.router) >>= (`Router.follow` f)
      Ask -> do
        dirty <- H.requestAll "editor" IsDirty
        modify $ \st -> st {answer = "dirty: " <> T.unwords (map show (M.elems dirty))}
      FromEditor Shouted -> do
        modify $ \st -> st {said = st.said <> ["shouted"]}
        H.raise (Said "shouted")
      FromEditor LeaveAsked -> H.raise Leave

----------------------------------------------------------------------
-- The editor, and its islands

data EditorQuery a = IsDirty (Bool -> a)

data EditorOutput = Shouted | LeaveAsked

data EditorAction = Shout | AskToLeave

editor :: forall m. (MonadIO m) => H.Component EditorQuery Int EditorOutput m
editor =
  H.component pure render [H.onAction handleAction, H.onQuery handleQuery]
  where
    render :: Int -> H.ComponentHTML EditorAction ("island" .== H.Slot (Const Void) Void Int) m
    render waitMs =
      HH.div
        [HP.class_ (HH.ClassName "editor")]
        ( [ HH.button [HP.class_ (HH.ClassName "shout"), HE.onClick (const Shout)] [HH.text "Shout"]
          , HH.button [HP.class_ (HH.ClassName "leave"), HE.onClick (const AskToLeave)] [HH.text "Leave"]
          ]
            <> [HH.slot_ "island" k island waitMs | k <- [1, 2]]
        )

    handleAction :: EditorAction -> H.HalogenM Int EditorAction ("island" .== H.Slot (Const Void) Void Int) EditorOutput m ()
    handleAction = \case
      Shout -> H.raise Shouted
      AskToLeave -> H.raise LeaveAsked

    handleQuery :: EditorQuery a -> H.HalogenM Int EditorAction ("island" .== H.Slot (Const Void) Void Int) EditorOutput m (Maybe a)
    handleQuery (IsDirty k) = pure (Just (k False))

data IslandState = Waiting Int | Loaded

-- | Waits when it starts, then shows it has loaded.
island :: forall q o m. (MonadIO m) => H.Component q Int o m
island =
  H.component
    (pure . Waiting)
    ( \st ->
        HH.span [HP.class_ (HH.ClassName "island")] [HH.text (case st of Waiting _ -> "waiting"; Loaded -> "loaded")] :: H.ComponentHTML () Empty m
    )
    [H.onAction load, H.onInitialize (load ())]
  where
    load :: () -> H.HalogenM IslandState () Empty o m ()
    load () =
      get >>= \case
        Waiting ms -> do
          -- On a worker, while the tree goes on.
          liftIO (js_islands_wait ms >>= void . await)
          put Loaded
        Loaded -> pass
