{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (put)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Net.Fetch qualified as Fetch

data Quote = Quote {quote :: Text, author :: Text}
  deriving (Generic)

instance Aeson.FromJSON Quote

data State a = Loading | Done a | Failed Text

data Action = Load Text

component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure Loading
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.initialize = Just (Load "data/quotes.json")}
      }
  where
    render state =
      HH.div_
        [ case state of
            Loading -> HH.p_ [HH.text "Loading..."]
            Failed message -> HH.p_ [HH.text ("Failed: " <> message)]
            Done quotes -> HH.ul_ [HH.li_ [HH.text (q.quote <> " (" <> q.author <> ")")] | q <- quotes]
        ]
    handleAction (Load url) = do
      put Loading
      result <- liftIO (getQuotes url)
      put (either Failed Done result)

-- Runs in IO, and waits: first for the response, then for its body.
getQuotes :: Text -> IO (Either Text [Quote])
getQuotes url = do
  fetched <- Fetch.fetch (Fetch.request Fetch.GET url)
  case fetched of
    Left err -> pure (Left (err.name <> ": " <> err.message))
    Right response
      | not response.ok -> pure (Left (url <> ": status " <> T.pack (show response.status)))
      | otherwise -> do
          body <- Fetch.text response
          pure $ case body of
            Left err -> Left err.message
            Right json -> either (Left . T.pack) Right (Aeson.eitherDecodeStrictText json)
