-- | Two Halogen components and the custom elements they are: @\<x-counter>@,
-- in a shadow root with styles of its own, which counts from its @start@
-- attribute by its @step@ property, dispatches a @count@ event at every
-- change and has the methods @reset()@ and @value()@; and @\<x-greeting>@,
-- rendered into the element itself, which greets its @name@ attribute.
module Example.CustomElement
  ( -- * The counter
    counter
  , counterDefinition
  , Settings (..)
  , CounterQuery (..)

    -- * The greeting
  , greeting
  , greetingDefinition
  )
where

import Data.Aeson (toJSON)
import Data.Row (Empty)
import Halogen qualified as H
import Halogen.CustomElement (Definition (..))
import Halogen.CustomElement qualified as CE
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude

-- | What the page sets on a counter: where it starts (the @start@
-- attribute, text as HTML has it) and how much a click adds (the @step@
-- property, a number, which JavaScript sets).
data Settings = Settings {start :: Int, step :: Int}
  deriving stock (Eq, Show)

data CounterQuery a
  = Reset a
  | GetValue (Int -> a)

data Counting = Counting {settings :: Settings, value :: Int}

data Action = Increase | Decrease
  deriving stock (Eq)

-- | A counter. A new @start@ starts it again there; a new @step@ changes
-- only what the next click adds. Every new value is raised.
counter :: forall m. (Monad m) => H.Component CounterQuery Settings Int m
counter =
  H.component
    (\settings -> pure Counting {settings, value = settings.start})
    render
    [H.onAction handleAction, H.onReceive receive, H.onQuery handleQuery]
  where
    render :: Counting -> H.ComponentHTML Action Empty m
    render st =
      HH.div
        [HP.class_ (HH.ClassName "counter")]
        [ HH.button [HE.onClick (const Decrease)] [HH.text ("−" <> show st.settings.step)]
        , HH.output_ [HH.text (show st.value)]
        , HH.button [HE.onClick (const Increase)] [HH.text ("+" <> show st.settings.step)]
        ]

    handleAction :: Action -> H.HalogenM Counting Action Empty Int m ()
    handleAction action = do
      modify $ \st -> st {value = st.value + (if action == Increase then 1 else -1) * st.settings.step}
      gets (.value) >>= H.raise

    receive :: Settings -> H.HalogenM Counting Action Empty Int m ()
    receive new = do
      old <- gets (.settings)
      modify $ \st -> st {settings = new}
      when (new.start /= old.start) $ do
        modify $ \st -> st {value = new.start}
        H.raise new.start

    handleQuery :: CounterQuery a -> H.HalogenM Counting Action Empty Int m (Maybe a)
    handleQuery = \case
      Reset next -> do
        modify $ \st -> st {value = st.settings.start}
        gets (.value) >>= H.raise
        pure (Just next)
      GetValue reply -> Just . reply <$> gets (.value)

-- | @\<x-counter start="3">@: its settings from the @start@ attribute and the
-- @step@ property, its values as @count@ events, its queries as methods, in
-- an open shadow root whose styles neither reach the page nor are reached
-- by it.
counterDefinition :: Definition CounterQuery Settings Int
counterDefinition =
  CE.defaultDefinition
    { input = \props ->
        Settings
          { start = fromMaybe 0 (CE.attr "start" props >>= readMaybe)
          , step = fromMaybe 1 (CE.prop "step" props)
          }
    , observed = ["start", "step"]
    , output = \value -> Just ("count", toJSON value)
    , methods = [CE.tellMethod "reset" (\() -> Reset), CE.method "value" (\() -> GetValue)]
    , -- One rule a line, joined rather than written with string gaps, which
      -- the C preprocessor (on in this package) would take apart.
      styles =
        unlines
          [ ".counter { display: inline-flex; align-items: center; gap: 0.5rem; padding: 0.25rem; border: 1px solid #94a3b8; border-radius: 0.5rem; }"
          , "button { font: inherit; min-width: 3rem; border: 0; border-radius: 0.25rem; padding: 0.25rem 0.5rem; background: #1e3a8a; color: white; cursor: pointer; }"
          , "output { min-width: 3rem; text-align: center; font-variant-numeric: tabular-nums; }"
          ]
    }

-- | A greeting, of whom its input names.
greeting :: forall q o m. (Monad m) => H.Component q Text o m
greeting = H.component pure (\name -> HH.p_ [HH.text ("Hello, " <> name <> "!")]) [H.onReceive put]

-- | @\<x-greeting name="reader">@, rendered into the element itself, among the
-- page's own elements, so that the page's CSS styles it.
greetingDefinition :: Definition q Text o
greetingDefinition =
  CE.defaultDefinition
    { input = fromMaybe "world" . CE.attr "name"
    , observed = ["name"]
    , shadow = CE.NoShadow
    }
