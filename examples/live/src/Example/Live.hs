-- | A board everyone who has it open writes on: a line typed and posted
-- shows up, at once, for everyone else too.
--
-- The component is an ordinary one, polymorphic in its monad. Served by
-- "Halogen.Remote.Server" each browser tab is a session with a board
-- component of its own, so what they share, the lines, is outside every
-- component: a 'Board' the program makes once, which each component reads
-- when it starts and whose changes it subscribes to.
module Example.Live
  ( Board
  , newBoard
  , post
  , board
  )
where

import Data.Row (Empty)
import Data.Text qualified as Text
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Subscription qualified as HS
import Protolude hiding (State)

-- | The lines, newest last, and who is told when they change.
data Board = Board
  { kept :: MVar [Text]
  , changes :: HS.Subscribe IO [Text]
  }

newBoard :: IO Board
newBoard = Board <$> newMVar [] <*> HS.create

-- | Add a line, keeping the last fifty, and tell every component showing the
-- board.
--
-- The lines are told while the board is held, so that what each component
-- hears comes in the order the lines were posted: two sessions posting at
-- once could otherwise make @[a, b]@ and then tell @[a]@ last, and every
-- page would show a board without @b@ until the next post. Telling does not
-- wait for the components (each hands its action to its own tree), so the
-- board is held only as long as it takes to hand them over.
post :: Board -> Text -> IO ()
post shared line = modifyMVar_ shared.kept $ \ls -> do
  let now = lastN 50 (ls <> [line])
  HS.notify shared.changes.listener now
  pure now
  where
    lastN n xs = drop (length xs - n) xs

data State = State
  { draft :: Text
  , lines :: [Text]
  }

data Action
  = Typed Text
  | Posted
  | Heard [Text]

board :: forall q m. (MonadIO m) => Board -> H.Component q () Void m
board shared =
  H.component
    (\() -> State "" <$> liftIO (readMVar shared.kept))
    render
    [ H.onInitialize (void (H.subscribe (Heard <$> shared.changes.emitter)))
    , H.onAction $ \case
        Typed text -> modify (\s -> s {draft = text})
        Heard ls -> modify (\s -> s {lines = ls})
        Posted -> do
          line <- Text.strip <$> gets (.draft)
          unless (Text.null line) $ liftIO (post shared line)
          modify (\s -> s {draft = ""})
    ]
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div
        [HP.class_ (H.ClassName "board")]
        [ HH.form
            [HE.onSubmit (const Posted)]
            [ HH.input [HP.value st.draft, HP.placeholder "Say something", HE.onValueInput Typed]
            , HH.button [] [HH.text "Post"]
            ]
        , HH.ul_ [HH.li_ [HH.text line] | line <- st.lines]
        , HH.p [HP.class_ (H.ClassName "count")] [HH.text (lineCount (length st.lines))]
        ]
    lineCount = \case
      0 -> "Nothing yet."
      1 -> "1 line."
      n -> show n <> " lines."
