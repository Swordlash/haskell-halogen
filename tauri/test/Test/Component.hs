-- | Commands and events in a component: a command sent in an effect and
-- waited for in @lift@ while the component goes on, and a subscription to an
-- event that goes with the component.
module Test.Component (spec) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (modify)
import Data.Aeson (FromJSON, ToJSON)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Tauri
import Test.Double (fire, listening, release, unlistens)
import Test.Hspec.Halogen

newtype Held = Held {name :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "sends a command in the click's dispatch, waits for it in lift, and renders meanwhile" $ runPage $ do
    ui <- mount m (desk :: H.Component H.VoidF () Void m) ()
    find ui ".start" >>= click
    answer <- find ui ".result"
    answer `shouldHaveText` "waiting"
    ticks <- find ui ".ticks"
    tick <- find ui ".tick"
    click tick
    ticks `shouldHaveText` "1"
    click tick
    ticks `shouldHaveText` "2"
    textContent answer `shouldReturn` "waiting"
    unsafeIOToPageM (release "component")
    eventually (answer `shouldHaveText` "component")

  it "hears an event it subscribed to, and stops listening when it goes away" $ runPage $ do
    -- Counted from what the tests before this one left on the bus.
    earlier <- unsafeIOToPageM unlistens
    listeners <- unsafeIOToPageM (listening "counter")
    ui <- mount m (desk :: H.Component H.VoidF () Void m) ()
    eventually (unsafeIOToPageM (listening "counter") `shouldReturn` listeners + 1)
    unsafeIOToPageM (fire "counter" (5 :: Int))
    counter <- find ui ".counter"
    eventually (counter `shouldHaveText` "5")
    unmount ui
    eventually (unsafeIOToPageM (listening "counter") `shouldReturn` listeners)
    unsafeIOToPageM unlistens `shouldReturn` earlier + 1

data State = State
  { result :: Text
  , ticks :: Int
  , counter :: Int
  }

data Action = Initialize | Start | Tick | Counted Int

-- | A component that sends a held command with @start@ in an effect and
-- waits for its @result@ in @lift@, counts clicks meanwhile, and shows the
-- last "counter" event it heard.
desk :: forall q o m. (MonadIO m) => H.Component q () o m
desk =
  H.mkComponent
    H.ComponentSpec
      { initialState = \() -> pure State {result = "idle", ticks = 0, counter = 0}
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div_
        [ button "start" Start
        , button "tick" Tick
        , HH.p [HP.class_ (HH.ClassName "result")] [HH.text st.result]
        , HH.p [HP.class_ (HH.ClassName "ticks")] [HH.text (Text.pack (show st.ticks))]
        , HH.p [HP.class_ (HH.ClassName "counter")] [HH.text (Text.pack (show st.counter))]
        ]

    button name action = HH.button [HP.class_ (HH.ClassName name), HE.onClick (const action)] [HH.text name]

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Initialize -> () <$ H.subscribe (Counted <$> listen "counter")
      Start -> do
        -- At once, inside the click's dispatch; nothing waits.
        sent <- H.liftEffect (start "held" Held {name = "component"})
        modify $ \st -> st {result = "waiting"}
        -- On a worker, while the tree goes on.
        outcome <- result sent :: H.HalogenM State Action Empty o m (Either TauriError Held)
        modify $ \st -> st {result = either (Text.pack . show) (.name) outcome}
      Tick -> modify $ \st -> st {ticks = st.ticks + 1}
      Counted n -> modify $ \st -> st {counter = n}
