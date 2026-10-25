{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (component) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.JSBits (inBrowser)

data State = State {count :: Int, ready :: Bool}

data Action = Initialize | Increment

-- A build renders this natively, in MemDOM, with renderToHtml component ();
-- the program around this module mounts it with hydrateUI rather than runUI.
component :: forall q o m. (MonadIO m) => H.Component q () o m
component =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure State {count = 0, ready = False}
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.initialize = Just Initialize}
      }
  where
    render st =
      HH.div_
        [ HH.p_ [HH.text "Count: ", HH.text (T.pack (show st.count))]
        , HH.button [HE.onClick (\_ -> Increment), HP.disabled (not st.ready)] [HH.text (if st.ready then "+" else "Loading...")]
        ]
    -- Natively, renderToHtml runs this before it takes the HTML: without
    -- inBrowser, the page would be written with a button that is ready.
    handleAction Initialize = when inBrowser (modify (\st -> st {ready = True}))
    handleAction Increment = modify (\st -> st {count = st.count + 1})
