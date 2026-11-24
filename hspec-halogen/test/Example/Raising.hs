-- | A component that raises an output at three moments: in its
-- @Initialize@, from a fork that its @Initialize@ starts, and when it is
-- told to, later. The first two happen while the component is being
-- mounted, before the harness hands the test the mounted component.
module Example.Raising (component, Query (..), Raised (..)) where

import Data.Row (Empty)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Protolude

-- | The output, named for the moment it was raised.
data Raised = Initializing | Forked | Told
  deriving stock (Eq, Show)

-- | Raise 'Told'.
newtype Query a = Tell a

component :: forall i m. (MonadIO m) => H.Component Query i Raised m
component =
  H.component (const (pure ())) render [H.onInitialize initialize, H.onQuery handleQuery]
  where
    render :: () -> H.ComponentHTML () Empty m
    render () = HH.p_ [HH.text "raising"]

    -- The fork waits in 'H.async' for a worker that only fills an MVar.
    -- The @Initialize@ then waits in place, on the loop, for that MVar, so
    -- the worker has handed the fork's continuation to the loop's queue
    -- before the first piece of the tree's work ends. The thread that
    -- mounts runs the queue to its end before it returns, so 'Forked' is
    -- raised before the mount returns, every time, rather than in a race
    -- with it. (A component's 'H.sync' must not wait; this one waits only
    -- for a worker that waits for nothing.)
    initialize :: H.HalogenM () () Empty Raised m ()
    initialize = do
      H.raise Initializing
      done <- H.sync (liftIO newEmptyMVar)
      void $ H.fork $ do
        H.async (liftIO (putMVar done ()))
        H.raise Forked
      H.sync (liftIO (takeMVar done))

    handleQuery :: Query a -> H.HalogenM () () Empty Raised m (Maybe a)
    handleQuery (Tell a) = Just a <$ H.raise Told
