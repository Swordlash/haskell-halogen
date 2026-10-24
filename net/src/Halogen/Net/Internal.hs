{-# LANGUAGE TemplateHaskell #-}

-- | What "Halogen.Net.WebSocket" and "Halogen.Net.EventSource" share: an
-- emitter that keeps what happens before anyone listens, the callbacks a
-- connection hands the browser, and reading what a browser constructor or
-- method threw.
module Halogen.Net.Internal
  ( Backlog
  , newBacklog
  , give
  , backlogEmitter
  , Callbacks
  , newCallbacks
  , callback
  , freeAll
  , refusal
  )
where

import Control.Monad (join, unless)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Sequence (Seq (..), (|>))
import Halogen.JSBits (Callback, JSText, JSVal, Safety (..), browserJS, freeCallback, fromJSText, mkSyncCallback)
import Halogen.JSBits.Promise (JSError (..))
import Halogen.Subscription qualified as HS

$( browserJS
     ["jsbits/net.js"]
     [ ("js_refused", "halogen_net_refused", Unsafe, [t|JSVal -> IO Bool|])
     , ("js_error_name", "halogen_net_error_name", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_error_message", "halogen_net_error_message", Unsafe, [t|JSVal -> IO JSText|])
     ]
 )

-- | An emitter that keeps what it is given while nobody has subscribed, and
-- hands all of it, in order, to its first subscriber as that subscribes.
-- From then on it is an ordinary 'HS.create' emitter: what it is given goes
-- to whoever is subscribed at the time, and to nobody if nobody is.
--
-- A connection starts as it is made, and the browser goes on with it
-- whether or not anyone has subscribed yet: without this, a component that
-- opened a socket in one action and subscribed in another (or a program
-- that opened it before handing it to a component) would miss whatever
-- arrived in between, its @open@ event included, and never know it had.
data Backlog a = Backlog
  { kept :: IORef (Maybe (Seq a))
  -- ^ What arrived before the first subscriber; 'Nothing' once that has
  -- subscribed and been handed all of it.
  , channel :: HS.Subscribe IO a
  }

newBacklog :: IO (Backlog a)
newBacklog = do
  kept <- newIORef (Just Empty)
  channel <- HS.create
  pure Backlog {kept, channel}

give :: Backlog a -> a -> IO ()
give backlog a = do
  held <- atomicModifyIORef' backlog.kept $ \case
    Just queue -> (Just (queue |> a), True)
    Nothing -> (Nothing, False)
  unless held (HS.notify backlog.channel.listener a)

-- | The first subscriber is registered before the backlog is handed over,
-- and the backlog is handed over one value at a time, each taken off the
-- queue as it goes. Whatever arrives meanwhile (a handler may run a
-- component's action, which may let the page run) joins the end of the
-- queue rather than overtaking it, and the queue becomes 'Nothing' only
-- once it is empty.
backlogEmitter :: Backlog a -> HS.Emitter IO a
backlogEmitter backlog = HS.Emitter $ \handler -> do
  subscription <- backlog.channel.emitter.registerHandler handler
  let drain = join $ atomicModifyIORef' backlog.kept $ \case
        Just (a :<| rest) -> (Just rest, HS.notify backlog.channel.listener a >> drain)
        _ -> (Nothing, pure ())
  drain
  pure subscription

-- | The callbacks a connection hands the browser, to free together once the
-- browser will call none of them again, and only once: freeing a callback
-- twice is undefined on both browser backends.
newtype Callbacks = Callbacks (IORef (Maybe [Callback]))

newCallbacks :: IO Callbacks
newCallbacks = Callbacks <$> newIORef (Just [])

-- | A callback the browser runs while it dispatches the event, as a DOM
-- listener is run, so that the emitter is given the event then, in the
-- order the browser dispatched the events in: the @open@ before the first
-- message, the last message before the @close@. An asynchronous callback
-- would hand each event to a thread of its own, and the order to the
-- scheduler.
callback :: Callbacks -> (JSVal -> IO ()) -> IO Callback
callback (Callbacks ref) handler = do
  made <- mkSyncCallback handler
  atomicModifyIORef' ref (\held -> (fmap (made :) held, ()))
  pure made

freeAll :: Callbacks -> IO ()
freeAll (Callbacks ref) = atomicModifyIORef' ref (Nothing,) >>= traverse_ (traverse_ freeCallback)

-- | What a binding that catches a browser's exception gave back: 'Nothing'
-- for anything that is not an error (the binding's result, or @null@), and
-- otherwise the error's @name@ and @message@, as
-- 'Halogen.JSBits.Promise.await' gives a rejection. Natively nothing is
-- ever refused.
refusal :: JSVal -> IO (Maybe JSError)
refusal value = do
  refused <- js_refused value
  if not refused
    then pure Nothing
    else do
      name <- fromJSText <$> js_error_name value
      message <- fromJSText <$> js_error_message value
      pure (Just JSError {name, message})
