-- | A component the page has not got yet: a wrapper that shows a placeholder
-- while a loader fetches the real component, then renders it in its place,
-- as React's @lazy@ and @Suspense@ do.
--
-- > import Halogen.Lazy qualified as Lazy
-- >
-- > HH.slot "editor" () (Lazy.lazy Lazy.defaultLoading loadEditor) document Edited
--
-- where @loadEditor :: m (H.Component Query Document Output m)@ is whatever
-- brings the component: code the page loads on demand (on WebAssembly, a
-- shared library linked into the running program, "Halogen.Lazy.Dyld"), a
-- request for what the component needs before it can show anything, or just
-- @pure editor@, which shows the component at once. From then on the loaded
-- component is an ordinary child of the wrapper: it gets the wrapper's
-- input, its output is the wrapper's, and the wrapper's queries reach it.
--
-- 'memo' shares one load between every mount of the wrapper, so that a
-- component loaded once (for one route, say) is there at once the next time
-- (another route, a second instance), without loading it again.
module Halogen.Lazy
  ( -- * The wrapper
    lazy
  , Loading (..)
  , defaultLoading

    -- * Loading once
  , memo
  )
where

import Control.Monad.Catch qualified as Catch
import Data.Row (type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Protolude hiding (State, modifyMVar, newMVar)
import UnliftIO (MonadUnliftIO)
import UnliftIO.MVar (modifyMVar, newMVar)

-- | What the wrapper shows until the component is there, and if it never
-- comes. Both are plain HTML, with no events: the wrapper has nothing to
-- handle until the component it waits for can.
data Loading = Loading
  { placeholder :: HH.PlainHTML
  -- ^ Shown from the first render until the loader has finished.
  , failed :: Text -> HH.PlainHTML
  -- ^ Shown in place of the component when the loader failed, given what it
  -- failed with ('displayException' of the exception).
  }

-- | An empty @\<div>@ while loading, and what the loader failed with in a
-- @\<pre>@.
defaultLoading :: Loading
defaultLoading =
  Loading
    { placeholder = HH.div_ []
    , failed = \message -> HH.pre_ [HH.text message]
    }

data State query input output m = State
  { input :: input
  , loaded :: Maybe (Either Text (H.Component query input output m))
  }

newtype Action output = Raise output

type Slots query output = ("inner" .== H.Slot query output ())

-- | A component that runs the loader when it is mounted, shows the
-- placeholder meanwhile, and then the loaded component in its place, with
-- the input the wrapper was given: the first one, and every one it has
-- received since, which the loaded component hears through its own
-- 'H.onReceive'. What the loaded component raises the wrapper raises, and
-- the queries the wrapper is sent are sent on to it.
--
-- A query that arrives before the component is there, or after the loader
-- failed, is answered @Nothing@, as one sent to a slot with nothing in it:
-- 'H.request' gives @Nothing@ and a 'H.tell' does nothing. The wrapper does
-- not keep it for later, since what a query asks of a component is usually
-- about now; a parent that must reach the component as soon as it is there
-- can ask again when the component's first output arrives, or have the
-- component raise one when it starts.
--
-- The loader runs once per mount, through 'lift', so it waits on a thread of
-- its own and the rest of the tree goes on meanwhile (a fetch, a link, a
-- delay). When it throws, the wrapper shows 'failed' with the exception, and
-- stays so until it is mounted again; an asynchronous exception (the wrapper
-- going away while the loader waits) is not caught. Removing the wrapper
-- while it loads stops waiting for the loader; 'memo' makes sure the next
-- mount does not load twice.
lazy
  :: forall query input output m
   . (Monad m)
  => Loading
  -> m (H.Component query input output m)
  -> H.Component query input output m
lazy Loading {placeholder, failed} loader =
  -- The state holds a component of any monad, which is not 'Typeable' for a
  -- polymorphic @m@, so the trace counts what the wrapper does without
  -- carrying its states (as "Halogen.Store"'s connect does).
  H.Component H.ComponentInfo {label = "Halogen.Lazy", stateType = Nothing} $
    H.componentSpec (\input -> pure State {input, loaded = Nothing}) render
      & H.onInitialize load
      & H.onAction handleAction
      & H.onReceive (\input -> modify (\current -> current {input}))
      & H.onQuery (H.query "inner" ())
  where
    render :: State query input output m -> H.ComponentHTML (Action output) (Slots query output) m
    render current = case current.loaded of
      Nothing -> HH.fromPlainHTML placeholder
      Just (Left message) -> HH.fromPlainHTML (failed message)
      Just (Right component) -> HH.slot "inner" () component current.input Raise

    load :: H.HalogenM (State query input output m) (Action output) (Slots query output) output m ()
    load = do
      result <- Catch.try (lift loader)
      case result of
        Left e
          | Just (_ :: SomeAsyncException) <- fromException e -> Catch.throwM e
          | otherwise -> modify (\current -> current {loaded = Just (Left (toS (displayException e)))})
        Right component -> modify (\current -> current {loaded = Just (Right component)})

    handleAction :: Action output -> H.HalogenM (State query input output m) (Action output) (Slots query output) output m ()
    handleAction (Raise output) = H.raise output

-- | Share one load between every use of it: the first to run it loads, and
-- every later one gets what it loaded, at once. While a load is under way, a
-- second use waits for it rather than loading again. A load that fails is
-- not kept: the use that ran it fails, and the next one loads again, so
-- that a wrapper mounted after a failed fetch tries it again.
--
-- > editor <- Lazy.memo loadEditor   -- once, say in the parent's initialState
-- > ... HH.slot "editor" () (Lazy.lazy Lazy.defaultLoading editor) document Edited
--
-- A use that is cancelled while it loads (its wrapper removed) leaves the
-- load to the next one.
memo :: forall a m n. (MonadUnliftIO m, MonadIO n) => m a -> n (m a)
memo load = do
  var <- newMVar Nothing
  pure $ modifyMVar var $ \case
    Just a -> pure (Just a, a)
    Nothing -> (\a -> (Just a, a)) <$> load
