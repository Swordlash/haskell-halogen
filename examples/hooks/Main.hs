{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}

-- | Every hook in @haskell-halogen-hooks@, in one page.
--
-- The whole application is one function from its input to its HTML. What it
-- needs to do that it asks for on the way down: state, effects, a memoised
-- value, a debouncer, a throttle, a handle on the latest value of something,
-- and a query to a child component.
module Main where

import Clay qualified as C
import DOM.HTML.Indexed qualified as I
import Data.Row (Empty, type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Hooks qualified as Hooks
import Halogen.Hooks.Extra.Actions.Events (preventDefault')
import Halogen.Hooks.Extra.Hooks (useDebouncer, useGet, useModifyState_, usePutState, useThrottle)
import Halogen.Hooks.Types (Hook, HookK (..), HookM)
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Protolude

#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
import Halogen.IO.Util qualified as HA
import Halogen.VDom.Driver (runUI)
#endif

----------------------------------------------------------------------
-- A hook of one's own.
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

----------------------------------------------------------------------
-- A child component, so the parent has something to query.
----------------------------------------------------------------------

newtype NotesQuery a = ReadNotes (Text -> a)

-- | An editable note. Its parent cannot see the text — it has to ask.
notes :: H.Component NotesQuery () Void BrowserDOM
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

type Html scope = Hooks.HookHTML scope Slots Output BrowserDOM

app :: H.Component H.VoidF () Output BrowserDOM
app = Hooks.component $ \_input -> Hooks.do
  counter <- useCounter 0

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

  Hooks.pure $
    page
      [ HH.h1_ [HH.text "Halogen Hooks"]
      , HH.p_ [HH.small_ [HH.text "Open the console: the effects say what they are doing."]]
      , panel
          "useState, useTickEffect, useMemo"
          "A hook of the page's own, plus a value that is only recomputed when the count changes."
          [ HH.div_
              [ button "-" counter.decrement
              , HH.text (" " <> show counter.count <> " ")
              , button "+" counter.increment
              ]
          , HH.p_ [HH.text ("count! = " <> show factorial)]
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

page :: forall scope. [Html scope] -> Html scope
page =
  HH.div
    [ HP.style $ do
        C.fontFamily ["system-ui", "sans-serif"] [C.sansSerif]
        C.maxWidth (C.px 680)
        C.marginLeft C.auto
        C.marginRight C.auto
        C.padding (C.px 24) (C.px 16) (C.px 24) (C.px 16)
    ]

panel :: forall scope. Text -> Text -> [Html scope] -> Html scope
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

button :: forall scope. Text -> HookM scope Slots Output BrowserDOM () -> Html scope
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
