{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}

-- | Every hook in @haskell-halogen-hooks@, in one page.
--
-- The whole application is one function from its input to its HTML. What it
-- needs to do that it asks for on the way down: state, effects, a memoised
-- value, a debouncer, a throttle, a handle on the latest value of something,
-- what a value was a render ago, somewhere to keep what should outlive the
-- page, and a query to a child component.
--
-- The component is written for any monad that is a browser, rather than for
-- 'BrowserDOM' itself, so that the module still compiles on a host GHC where
-- there is no browser to be had. Only 'main' names the backend.
module Main where

import Clay qualified as C
import DOM.HTML.Indexed qualified as I
import Data.Map.Strict qualified as M
import Data.Row (Empty, type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Extra.Actions.Events (preventDefault')
import Halogen.Hooks.Extra.Hooks (StorageInterface (..), useDebouncer, useGet, useLocalStorage, useModifyState_, usePrevious, usePutState, useThrottle)
import Halogen.Hooks.Types (Hook, HookK (..), HookM)
import Halogen.Query.Event qualified as HQE
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (BrowserDOM, MonadBrowserDOM, document, runBrowserDOM, window, windowToEventTarget)
import Protolude
import UnliftIO (MonadUnliftIO)
import Web.Event.Event (EventType (..))
import Web.HTML.Cookie qualified as Cookie
import Web.HTML.Window qualified as Window

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Halogen.IO.Util qualified as HA
import Halogen.VDom.Driver (runUI)
#endif

----------------------------------------------------------------------
-- Hooks of the page's own.
----------------------------------------------------------------------

-- | A composite hook is a type synonym over the hooks it is made of, and the
-- list reads in the order they are used.
type UseCounter hooks = UseState Int : UseEffect Int : hooks

-- | What 'useCounter' hands back. Handlers rather than a 'Hooks.StateId', so
-- the caller cannot change the count in a way the hook did not intend.
data Counter scope slots output m = Counter
  { count :: Int
  , increment :: HookM scope slots output m ()
  , decrement :: HookM scope slots output m ()
  }

-- | A counter that logs every value it takes.
--
-- Nothing here is special to this application: a hook is an ordinary function,
-- and this one could live in a library.
useCounter
  :: forall scope q slots output m hooks
   . (MonadIO m)
  => Int
  -> Hook scope q slots output m (UseCounter hooks) hooks (Counter scope slots output m)
useCounter initial = Hooks.do
  (count, countId) <- Hooks.useState initial

  Hooks.useTickEffect count $ do
    liftIO $ putStrLn ("[effect] the count is now " <> show count :: Text)
    pure Nothing

  Hooks.pure
    Counter
      { count
      , increment = Hooks.modify_ countId (+ 1)
      , decrement = Hooks.modify_ countId (subtract 1)
      }

-- | The width of the window, kept current.
--
-- This one stays here rather than in @Halogen.Hooks.Extra@: what to do about
-- resize events is an application's business — throttle them, ignore anything
-- under a threshold, watch a different window — and everything it is made of
-- is library already. It is also the shortest example of subscribing a hook to
-- a DOM event: 'HQE.eventListener' speaks the component's monad and the driver
-- subscribes in 'IO', which is what 'HS.lowerEmitter' is for.
type UseWindowWidth hooks = UseState (Maybe Int) : UseEffect () : hooks

useWindowWidth
  :: forall scope q slots output m hooks
   . (MonadIO m, MonadUnliftIO m, MonadBrowserDOM m)
  => Hook scope q slots output m (UseWindowWidth hooks) hooks (Maybe Int)
useWindowWidth = Hooks.do
  (width, setWidth) <- usePutState Nothing

  Hooks.useLifecycleEffect $ do
    win <- lift window
    target <- lift (windowToEventTarget win)
    resizes <- lift $ HS.lowerEmitter $ HQE.eventListener (EventType "resize") target (const (Just ()))

    let measure = setWidth . Just =<< lift (Window.innerWidth win)
    subscription <- Hooks.subscribe $ map (const measure) resizes
    measure

    pure $ Just $ Hooks.unsubscribe subscription

  Hooks.pure width

----------------------------------------------------------------------
-- A child component, so the parent has something to query.
----------------------------------------------------------------------

newtype NotesQuery a = ReadNotes (Text -> a)

-- | An editable note. Its parent cannot see the text — it has to ask.
notes :: forall m. (MonadIO m) => H.Component NotesQuery () Void m
notes = Hooks.component @Empty $ \_input -> Hooks.do
  (text, setText) <- usePutState "hooks all the way down"

  -- The handler is replaced on every render, so closing over this render's
  -- @text@ is safe: by the time a query arrives, it is the current one.
  Hooks.useQuery $ \case
    ReadNotes reply -> pure $ Just $ reply text

  Hooks.pure $
    HH.input
      [ HP.type_ I.InputText
      , HP.value text
      , HP.style $ C.width (C.pct 100)
      , HE.onValueInput setText
      ]

----------------------------------------------------------------------
-- The application.
----------------------------------------------------------------------

-- | Raised whenever the counter changes, and logged by 'main'.
newtype Output = Counted Int

type Slots = "notes" .== H.Slot NotesQuery Void ()

type Html scope m = Hooks.HookHTML scope Slots Output m

app :: forall m. (MonadIO m, MonadUnliftIO m, MonadBrowserDOM m) => H.Component H.VoidF () Output m
app = Hooks.component $ \_input -> Hooks.do
  counter <- useCounter 0

  -- What the count was a render ago, which this render has no other way to
  -- know.
  previousCount <- usePrevious counter.count

  -- A value read from a program that outlives the render it was written in.
  getCount <- useGet counter.count

  -- Recomputed only when the count changes, however often the page renders.
  factorial <- Hooks.useMemo counter.count $ \n -> product [1 .. max 1 (toInteger n)]

  Hooks.useTickEffect counter.count $ do
    Hooks.raise (Counted counter.count)
    pure Nothing

  Hooks.useLifecycleEffect $ do
    -- A fork of the component: killed with it, or by the cleanup below.
    ticker <- Hooks.fork $ forever $ do
      liftIO $ threadDelay 5_000_000
      current <- getCount
      liftIO $ putStrLn ("[fork] five seconds on, the count is " <> show current :: Text)
    pure $ Just $ Hooks.kill ticker

  (typed, setTyped) <- usePutState ""
  (settled, setSettled) <- usePutState ""
  settle <- useDebouncer 0.4 setSettled

  (clicks, countClick) <- useModifyState_ (0 :: Int)
  (accepted, countAccepted) <- useModifyState_ (0 :: Int)
  acceptClick <- useThrottle 1 $ \() -> countAccepted (+ 1)

  (fromChild, setFromChild) <- usePutState "(not asked yet)"

  -- Kept in the browser rather than in the component: reload the page and the
  -- note is still here.
  (kept, setKept) <-
    useLocalStorage
      StorageInterface
        { key = "halogen-hooks-example/note"
        , defaultValue = "" :: Text
        , encode = identity
        , decode = Right
        }

  (jar, setJar) <- usePutState M.empty
  width <- useWindowWidth

  let readJar = setJar =<< lift (Cookie.getCookies =<< document =<< window)

  Hooks.useLifecycleEffect $ readJar $> Nothing

  Hooks.pure $
    page
      [ HH.h1_ [HH.text "Halogen Hooks"]
      , HH.p_ [HH.small_ [HH.text "Open the console: the effects say what they are doing."]]
      , panel
          "useState, useTickEffect, useMemo, usePrevious"
          "A hook of the page's own, a value only recomputed when the count changes, and what the count was a render ago."
          [ HH.div_
              [ button "-" counter.decrement
              , HH.text (" " <> show counter.count <> " ")
              , button "+" counter.increment
              ]
          , HH.p_ [HH.text ("count! = " <> show factorial)]
          , HH.p_ [HH.text ("a render ago: " <> maybe "(nothing yet)" show previousCount)]
          ]
      , panel
          "useDebouncer, preventDefault"
          "The second line catches up once the typing has stopped for 400ms, or at once on Enter."
          [ HH.form
              [ HE.onSubmit $ \event -> do
                  -- Submitting a form navigates, and navigating would throw
                  -- the whole component away and start it again.
                  preventDefault' event
                  setSettled typed
              ]
              [ HH.input
                  [ HP.type_ I.InputText
                  , HP.placeholder "type here"
                  , HP.value typed
                  , HP.style $ C.width (C.pct 100)
                  , HE.onValueInput $ \t -> do
                      setTyped t
                      settle t
                  ]
              , HH.button [] [HH.text "settle now"]
              ]
          , HH.p_ [HH.text ("typing:  " <> typed)]
          , HH.p_ [HH.text ("settled: " <> settled)]
          ]
      , panel
          "useThrottle"
          "Click as fast as you like; the throttled count moves at most once a second."
          [ button "click me" $ do
              countClick (+ 1)
              acceptClick ()
          , HH.p_ [HH.text ("clicks: " <> show clicks <> ", throttled: " <> show accepted)]
          ]
      , panel
          "useLocalStorage, and a cookie"
          "The note is read back out of the browser on mount and written on every change: reload the page and it is still here."
          [ HH.input
              [ HP.type_ I.InputText
              , HP.placeholder "something to keep"
              , HP.value (either (const "") identity kept)
              , HP.style $ C.width (C.pct 100)
              , HE.onValueInput $ \t -> setKept (const (Right t))
              ]
          , HH.p_ [HH.text (either ("what was kept could not be read: " <>) ("what was kept: " <>) kept)]
          , HH.p_
              [ button "keep the count in a cookie" $ do
                  doc <- lift (document =<< window)
                  lift $
                    Cookie.setCookie
                      (Cookie.defaultCookie "halogen-hooks-example" (show counter.count))
                        { Cookie.maxAge = Just 3600
                        , Cookie.path = Just "/"
                        , Cookie.sameSite = Just Cookie.Lax
                        }
                      doc
                  readJar
              ]
          , HH.p_ [HH.small_ [HH.text ("cookies: " <> showJar jar)]]
          ]
      , panel
          "a hook of your own: useWindowWidth"
          "Subscribed to the window's resize event for as long as the component is mounted. Drag the window."
          [ HH.p_ [HH.text ("window width: " <> maybe "(measuring)" show width)]
          ]
      , panel
          "useQuery, in a child"
          "The note below is a second hooks component; this one has to ask it for its text."
          [ HH.slot_ "notes" () notes ()
          , HH.p_
              [ button "ask the note" $ do
                  answer <- Hooks.query "notes" () (ReadNotes identity)
                  setFromChild (fromMaybe "(the note did not answer)" answer)
              ]
          , HH.p_ [HH.text ("the note says: " <> fromChild)]
          ]
      ]

----------------------------------------------------------------------
-- Chrome.
----------------------------------------------------------------------

showJar :: Map Text Text -> Text
showJar jar
  | M.null jar = "(none)"
  | otherwise = T.intercalate ", " [k <> "=" <> v | (k, v) <- M.toList jar]

page :: forall scope m. [Html scope m] -> Html scope m
page =
  HH.div
    [ HP.style $ do
        C.fontFamily ["system-ui", "sans-serif"] [C.sansSerif]
        C.maxWidth (C.px 680)
        C.marginLeft C.auto
        C.marginRight C.auto
        C.padding (C.px 24) (C.px 16) (C.px 24) (C.px 16)
    ]

panel :: forall scope m. Text -> Text -> [Html scope m] -> Html scope m
panel title subtitle contents =
  HH.section
    [ HP.style $ do
        C.border (C.px 1) C.solid (C.rgb 0xd0 0xd0 0xd0)
        C.padding (C.px 8) (C.px 16) (C.px 16) (C.px 16)
        C.marginBottom (C.px 16)
    ]
    ( [ HH.h2_ [HH.text title]
      , HH.p_ [HH.small_ [HH.text subtitle]]
      ]
        <> contents
    )

button :: forall scope m. Text -> HookM scope Slots Output m () -> Html scope m
button label act = HH.button [HE.onClick $ const act] [HH.text label]

----------------------------------------------------------------------
-- Starting it up.
----------------------------------------------------------------------

attachComponent :: BrowserDOM (H.HalogenSocket H.VoidF Output BrowserDOM)

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
attachComponent = HA.awaitBody >>= runUI app ()
#else
attachComponent = panic "This example runs in a browser: build it with the JavaScript or wasm backend."
#endif

main :: IO ()
main = runBrowserDOM $ do
  H.HalogenSocket {messages} <- attachComponent
  void $ HS.subscribe messages $ \(Counted n) ->
    liftIO $ putStrLn ("[output] the counter reached " <> show n :: Text)

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" start :: IO ()

start :: IO ()
start = main
#endif
