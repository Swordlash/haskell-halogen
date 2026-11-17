{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Latest (Ask, Asks, newAsk, noAsks, supersede)
import Halogen.Machine qualified as M
import Halogen.Sync qualified as Sync
import Web.HTML.HTMLElement qualified as HTMLElement

-- The view's logic. It is plain Haskell: nothing here is Halogen's.
data Model = Model {typed :: Text, shown :: Text, results :: [Text], searches :: Asks}

data Event = Typed Text | Paused Text | Found Ask Text [Text] | Cleared

data Effect = Pause Text | StopPause | Search Ask Text | FocusField

step :: Event -> Model -> (Model, [Effect])
step event model = case event of
  Typed text -> (model {typed = text}, [Pause text])
  Paused text
    | T.null text -> (cleared model, [])
    | otherwise ->
        let (ticket, later) = newAsk model.searches
         in (model {searches = later}, [Search ticket text])
  Found _ text found -> (model {shown = text, results = found}, [])
  Cleared -> ((cleared model) {typed = ""}, [StopPause, FocusField])

-- No words shown, and no answer still out may show.
cleared :: Model -> Model
cleared model = model {shown = "", results = [], searches = supersede model.searches}

-- The server. It answers a short query more slowly than a long one.
search :: Text -> IO [Text]
search text = do
  threadDelay (if T.length text < 2 then 900000 else 300000)
  pure (filter (T.isPrefixOf (T.toLower text)) dictionary)

dictionary :: [Text]
dictionary = ["abacus", "able", "about", "acorn", "actor", "after", "bacon", "cabin"]

-- The component keeps the machine, and the field's ref for the focus.
data State = State {machine :: M.Machine Pausing Event Model, field :: H.RefLabel}

data Pausing = Pausing
  deriving (Eq, Ord)

component :: forall q o m. (MonadIO m) => H.Component q () o m
component = H.component initialState render [H.onAction (M.happen spec)]
  where
    initialState _ = do
      label <- liftIO (H.newRefLabel "search")
      pure State {machine = M.new (Model "" "" [] noAsks), field = label}
    spec = M.Spec {M.step = step, M.carry = carry, M.get = (.machine), M.set = \m s -> s {machine = m}}
    carry = \case
      Pause text -> M.AskUnder Pausing (M.delayCounted 0.3) (const (Paused text))
      StopPause -> M.Cancel Pausing
      Search ticket text -> M.Ask (M.run (liftIO (search text))) (Found ticket text . either (const []) id)
      FocusField -> M.Now $ do
        found <- gets (.field) >>= Sync.getRef
        traverse_ (Sync.sync . liftIO . HTMLElement.focus) (found >>= HTMLElement.fromElement)
    render state =
      let model = M.current state.machine
       in HH.div_
            [ HH.input [HP.ref state.field, HP.value model.typed, HE.onValueInput Typed]
            , HH.button [HE.onClick (const Cleared)] [HH.text "Clear"]
            , HH.p_ [HH.text (if T.null model.shown then "Type a word." else "Words for \"" <> model.shown <> "\":")]
            , HH.ul_ [HH.li_ [HH.text word] | word <- model.results]
            ]
