-- | Requests a component makes: one started in an effect and waited for in
-- @lift@, one aborted from another action, and one that goes with its
-- component.
module Test.Component (spec) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (gets, modify)
import Data.Foldable (traverse_)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Net.Fetch
import Test.Hspec.Halogen
import Test.Network (aborted, fixtureText, release, started)

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "starts a request in the click's dispatch, waits for it in lift, and renders meanwhile" $ runPage $ do
    ui <- mount m (loader :: H.Component H.VoidF Text Void m) "held/pattern/fixture.txt"
    find ui ".start" >>= click
    -- The request was made in the click's own handling, before it settled.
    unsafeIOToPageM (started "pattern") `shouldReturn` True
    result <- find ui ".result"
    result `shouldHaveText` "loading"
    ticks <- find ui ".ticks"
    tick <- find ui ".tick"
    click tick
    ticks `shouldHaveText` "1"
    click tick
    ticks `shouldHaveText` "2"
    textContent result `shouldReturn` "loading"
    unsafeIOToPageM (release "pattern")
    eventually (result `shouldHaveText` fixtureText)

  it "aborts a request it keeps from another action" $ runPage $ do
    ui <- mount m (loader :: H.Component H.VoidF Text Void m) "held/kept-in-state/fixture.txt"
    find ui ".start" >>= click
    find ui ".abort" >>= click
    find ui ".result" >>= (`shouldHaveText` "AbortError")
    unsafeIOToPageM (aborted "kept-in-state") `shouldReturn` True

  it "aborts the request of a fetch in lift when the component goes away" $ runPage $ do
    ui <- mount m (loader :: H.Component H.VoidF Text Void m) "held/unmounted/fixture.txt"
    find ui ".load" >>= click
    find ui ".result" >>= (`shouldHaveText` "loading")
    unsafeIOToPageM (started "unmounted") `shouldReturn` True
    unmount ui
    eventually (unsafeIOToPageM (aborted "unmounted") `shouldReturn` True)

data State = State
  { address :: Text
  , pending :: Maybe Fetch
  , result :: Text
  , ticks :: Int
  }

data Action = Start | Load | Abort | Tick

-- | A component that loads the text at an address: with @start@ in an
-- effect and @response@ in @lift@, keeping the request to abort it, or with
-- @fetch@ in @lift@.
loader :: forall q o m. (MonadIO m) => H.Component q Text o m
loader =
  H.mkComponent
    H.ComponentSpec
      { initialState = \address -> pure State {address, pending = Nothing, result = "idle", ticks = 0}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div_
        [ button "start" Start
        , button "load" Load
        , button "abort" Abort
        , button "tick" Tick
        , HH.p [HP.class_ (HH.ClassName "result")] [HH.text st.result]
        , HH.p [HP.class_ (HH.ClassName "ticks")] [HH.text (Text.pack (show st.ticks))]
        ]

    button name action = HH.button [HP.class_ (HH.ClassName name), HE.onClick (const action)] [HH.text name]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Start -> do
        address <- gets (.address)
        -- At once, inside the click's dispatch; nothing waits.
        started' <- H.liftEffect (start (request GET address))
        modify $ \st -> st {pending = Just started', result = "loading"}
        -- On a worker, while the tree goes on.
        shown =<< (response started' >>= either (pure . Left) text)
      Load -> do
        address <- gets (.address)
        modify $ \st -> st {result = "loading"}
        shown =<< (fetch (request GET address) >>= either (pure . Left) text)
      Abort -> gets (.pending) >>= traverse_ (H.liftEffect . abort)
      Tick -> modify $ \st -> st {ticks = st.ticks + 1}

    shown outcome = modify $ \st -> st {result = either (.name) id outcome}
