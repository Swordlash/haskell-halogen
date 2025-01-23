{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | A container component which renders a sub-tree to a DOM node not in the
-- | tree. This is useful for when a child component needs to 'break out' of a
-- | parent, like dialogs, modals, and tooltips, especially if the parent has
-- | z-indexing or overflow: hidden set.
module Halogen.Portal where

import Control.Monad.Fork
import Control.Monad.Parallel
import Control.Monad.UUID
import Data.Functor.Coyoneda (Coyoneda (..), hoistCoyoneda)
import Data.NT
import Data.Row
import HPrelude hiding (State)
import Halogen as H hiding (ChildQuery, State)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.IO.Util (awaitBody)
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad
import Halogen.VDom.Driver as VDom
import Web.DOM.Internal.Types

data Input query input output m = Input
  { input :: input
  , child :: H.Component query input output m
  , targetElement :: Maybe HTMLElement
  }

data State query input output m = State
  { socket :: Maybe (H.HalogenSocket (Query input query) output m)
  , input :: input
  , child :: H.Component query input output m
  , targetElement :: Maybe HTMLElement
  }

portal
  :: forall label
    ->forall query action input output slots slot m
   . ( HasType label (H.Slot query output slot) slots
     , MonadUnliftIO m
     , MonadDOM m
     , MonadKill m
     , MonadParallel m
     , MonadMask m
     , MonadUUID m
     )
  => (KnownSymbol label)
  => (Ord slot)
  => slot
  -> H.Component query input output m
  -> input
  -> Maybe HTMLElement
  -> (output -> action)
  -> H.ComponentHTML action slots m
portal label' slot' child input' htmlElement handler' =
  HH.slot
    label'
    slot'
    component
    ( Input
        { child
        , input = input'
        , targetElement = htmlElement
        }
    )
    handler'

data Query input query a
  = SetInput input a
  | ChildQuery (query a)

-- wraps the portalled component and provides a SetInput query
-- that can be used by the Portal component to update the child's
-- input when it receives new values from the parent
wrapper
  :: forall query input output m
   . (MonadIO m)
  => H.Component (Query input query) (State query input output m) output m
wrapper =
  H.mkComponent
    $ H.ComponentSpec
      { initialState = pure
      , render
      , eval =
          H.mkEval
            $ H.defaultEval
              { H.handleQuery = handleQuery
              , H.handleAction = H.raise
              }
      }
  where
    render (State {input, child}) =
      HH.div
        [HP.styleText "display: contents"]
        [HH.slot "content" () child input identity]

    handleQuery
      :: forall action a
       . Query input query a
      -> H.HalogenM (State query input output m) action ("content" .== H.Slot _ _ _) output m (Maybe a)
    handleQuery = \case
      SetInput input a -> do
        modify $ \s -> s {input} :: State query input output m
        pure $ Just a
      ChildQuery query -> do
        res <- H.query "content" () query
        pure res

component
  :: forall q i o m
   . (MonadDOM m, MonadUnliftIO m, MonadKill m, MonadParallel m, MonadMask m, MonadUUID m)
  => H.Component q (Input q i o m) o m
component =
  H.mkComponent
    $ H.ComponentSpec
      { initialState = \Input {..} -> pure State {socket = Nothing, ..}
      , render
      , eval
      }
  where
    eval
      :: H.HalogenQ q o (Input q i o m) ~> H.HalogenM (State q i o m) o Empty o m
    eval = NT $ \case
      H.Initialize a -> do
        state <- get
        UnliftIO f <- lift askUnliftIO
        -- The target element can either be the one supplied by the user, or the
        -- document body. Either way, we'll run the sub-tree at the target and
        -- save the resulting interface.
        target <- maybe (lift awaitBody) pure state.targetElement
        socket@H.HalogenSocket {messages = HS.Emitter k} <- lift $ VDom.runUI wrapper state target
        -- Subscribe to the child component's messages
        void $ H.subscribe $ HS.Emitter $ \emit' ->
          fmap (HS.hoistSubscription (NT f)) $ f $ k $ liftIO . emit'
        modify $ \s -> s {socket = Just socket} :: State q i o m
        pure a
      H.Finalize a -> do
        gets (.socket) >>= traverse_ (lift . (.dispose))
        pure a
      H.Receive (Input {input}) a ->
        gets (.socket)
          >>= \case
            Nothing -> pure a
            Just io -> do
              void $ lift $ ioq io (SetInput input a)
              pure a
      H.Action output a -> do
        H.raise output
        pure a
      H.Query query fail ->
        gets (.socket)
          >>= \case
            Nothing -> pure $ fail ()
            Just io -> lift $ case hoistCoyoneda ChildQuery query of
              Coyoneda k q -> maybe (fail ()) k <$> ioq io q

    -- We don't need to render anything; this component is explicitly meant to be
    -- passed through.
    render :: State q i o m -> H.ComponentHTML o Empty m
    render _ = HH.text ""

    -- This is needed for a hint to the typechecker. Without it there's an
    -- impredicativity issue with `a` when `HalogenIO` is taken from `State`.
    ioq :: forall a. H.HalogenSocket (Query i q) o m -> Query i q a -> m (Maybe a)
    ioq H.HalogenSocket {query} = query
