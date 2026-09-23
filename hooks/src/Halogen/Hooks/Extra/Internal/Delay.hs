-- | One place to turn a duration into a wait.
module Halogen.Hooks.Extra.Internal.Delay (delayFor) where

import Data.Time (NominalDiffTime)
import Protolude

-- | Wait for the given time. Written in 'NominalDiffTime' because that is what
-- the rest of the library takes for a duration.
delayFor :: forall m. (MonadIO m) => NominalDiffTime -> m ()
delayFor d = liftIO $ threadDelay $ max 0 $ floor $ realToFrac @_ @Double d * 1e6
