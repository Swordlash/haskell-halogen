{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- The program around lesson 21 mounts the lesson's component in RemoteDOM,
-- through Halogen.Remote.Loopback: its operations go, as JSON, to a client
-- in an element of this page, the page's events come back the same way, and
-- a page component of its own shows each message under the result as it
-- crosses, newest first. What it keeps for the next run ends that page
-- component, whose finalizer ends the loopback.
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify)
import Data.Foldable (for_)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen (HalogenSocket (..))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.IO.Util qualified as HA
import Halogen.Remote.Loopback qualified as Loopback
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Lesson (component)
import Web.DOM.ParentNode (QuerySelector (..))

data Wire = Wire {container :: H.RefLabel, running :: Maybe Loopback.Loopback, seen :: [Text]}

newtype Crossed = Crossed Loopback.Traffic

wire :: H.Component q () o BrowserDOM
wire = H.component start render [H.onInitialize mountIt, H.onFinalize stop, H.onAction heard]
  where
    start _ = (\label -> Wire {container = label, running = Nothing, seen = []}) <$> H.newRefLabel "lesson-wire"
    render :: Wire -> H.ComponentHTML Crossed Empty BrowserDOM
    render st =
      HH.div_
        [ HH.div [HP.ref st.container, HP.class_ (H.ClassName "lesson-remote")] []
        , HH.p_ [HH.text "The wire, newest first:"]
        , -- Wrapped, so that a message as wide as the first render's
          -- reads in the narrow result's column.
          HH.pre
            [ HP.class_ (H.ClassName "lesson-wire")
            , HP.styleText "white-space: pre-wrap; overflow-wrap: anywhere; font-size: 0.75rem; max-height: 28rem; overflow-y: auto"
            ]
            [HH.text (T.unlines st.seen)]
        ]
    mountIt = do
      st <- get
      element <- H.getHTMLElementRef st.container
      for_ element $ \container -> do
        loopback <- liftIO (Loopback.mountIn container component ())
        _ <- H.subscribe (Crossed <$> loopback.traffic)
        modify (\s -> s {running = Just loopback})
    stop = do
      st <- get
      for_ st.running (\loopback -> liftIO loopback.dispose)
    heard (Crossed message) = modify (\s -> s {seen = take 12 (describe message : s.seen)})
    describe = \case
      Loopback.ToPage text -> "to the page:    " <> shorten text
      Loopback.ToProgram text -> "to the program: " <> shorten text
    shorten text = if T.length text > 600 then T.take 600 text <> " …" else text

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  result <- HA.selectElement (QuerySelector "#learn-result") >>= maybe (error "the page has no #learn-result") pure
  socket <- runUI wire () result
  HA.keepForNextRun socket.dispose
