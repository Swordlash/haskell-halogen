{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | A JavaScript promise, kept to be awaited later.
--
-- A binding that starts asynchronous work (a request, a timer, a dialog)
-- returns a 'Promise' at once, as an 'Unsafe' binding: the JavaScript has
-- started the work by the time it returns, and nothing has waited.
--
-- > $(browserJS ["jsbits/net.js"]
-- >   [("js_fetch_text", "halogen_net_fetch_text", Unsafe, [t| JSText -> IO Promise |])])
--
-- 'await' is the one place that waits for it, on every backend the same
-- way, and gives a rejection back as a value. That separates starting the
-- work from waiting for it, which a 'Safe' binding ties together. A
-- component can start work in @H.liftEffect@, inside the dispatch of the
-- event that asked for it, and keep the 'Promise' in its state, then 'await'
-- it in @lift@ (on a worker, while the rest of the tree goes on), or never.
-- It can start several and 'awaitAll' of them at once.
--
-- A 'Promise' is untyped on purpose for now: what it resolves with is a
-- 'JSVal', for the binding's wrapper to decode itself.
--
-- A module declaring such a binding imports @Promise (..)@: the FFI passes a
-- newtype only when its constructor is in scope.
--
-- A promise that is rejected and never awaited is reported by the engine as
-- an unhandled rejection, which a browser logs and Node ends its process
-- for, so the JavaScript of a binding whose result may be dropped, and whose
-- failure does not matter then, should handle the rejection itself.
--
-- This module declares its own bindings with 'browserJS', so it sits above
-- "Halogen.JSBits" rather than being re-exported from it; import both.
module Halogen.JSBits.Promise
  ( Promise (..)
  , JSError (..)
  , await
  , awaitAll
  )
where

import Control.Exception (Exception (..), evaluate)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Halogen.JSBits (Safety (..), browserJS)
import Halogen.JSBits.Value (Inert, JSText, JSVal, fromJSText, inBrowser)

-- | A JavaScript promise, or any value, which then stands for a promise that
-- has resolved with it. Natively, where no engine makes one, a placeholder,
-- which 'await' never waits for.
newtype Promise = Promise JSVal
  deriving newtype (Inert)

-- | What a promise was rejected with: an @Error@'s @name@ and @message@ (a
-- @DOMException@ is one, so a @TypeError@ or an @AbortError@ keeps its
-- name). Anything else a promise is rejected with, a string say, has the
-- name @Error@ and itself, as a string, as its message.
--
-- Natively 'await' gives the name @NotInBrowserError@, since there is no
-- engine to run the work at all.
data JSError = JSError
  { name :: Text
  , message :: Text
  }
  deriving stock (Eq, Show)

instance Exception JSError where
  displayException e = Text.unpack (e.name <> ": " <> e.message)

-- The outcome of a wait is an object of jsbits/promise.js's, holding the
-- promise until it settles and then what it settled with: the one awaited
-- binding on the JavaScript backend may only return nothing (see
-- 'browserJS'), so what it resolved with is read afterwards.
$( browserJS
     ["jsbits/promise.js"]
     [ ("js_outcome", "halogen_promise_outcome", Unsafe, [t|Promise -> IO JSVal|])
     , ("js_settle", "halogen_promise_settle", Safe, [t|JSVal -> IO ()|])
     , ("js_outcomes", "halogen_promise_outcomes", Unsafe, [t|IO JSVal|])
     , ("js_push", "halogen_promise_push", Unsafe, [t|JSVal -> JSVal -> IO ()|])
     , ("js_settle_all", "halogen_promise_settle_all", Safe, [t|JSVal -> IO ()|])
     , ("js_ok", "halogen_promise_ok", Unsafe, [t|JSVal -> IO Bool|])
     , ("js_value", "halogen_promise_value", Unsafe, [t|JSVal -> IO JSVal|])
     , ("js_name", "halogen_promise_name", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_message", "halogen_promise_message", Unsafe, [t|JSVal -> IO JSText|])
     ]
 )

-- | Wait for the promise to settle, and give what it resolved with, or what
-- it was rejected with. It never throws a rejection, on any backend.
--
-- Waiting is all it does. Killing the thread (or the component's fiber)
-- that waits stops the wait, not the work: a promise cannot be cancelled,
-- and whatever started it goes on to the end, its outcome unread. A binding
-- for an API that takes an @AbortSignal@ (@fetch@ does) should take one
-- itself and hand it on, so that the program can abort the work as well as
-- stop waiting for it.
--
-- Any number of waits on the same promise each get what it settled with,
-- and so does one that starts after it has settled.
--
-- Natively it waits for nothing and gives @'JSError' "NotInBrowserError" _@.
await :: (MonadIO m) => Promise -> m (Either JSError JSVal)
await promise
  | not inBrowser = pure (Left notInBrowser)
  | otherwise = liftIO $ do
      outcome <- js_outcome promise
      settle (js_settle outcome)
      outcomeOf outcome

-- | Wait for every promise to settle, with one wait for all of them rather
-- than one after another, and give each one's outcome, in order, as 'await'
-- would.
awaitAll :: (MonadIO m) => [Promise] -> m [Either JSError JSVal]
awaitAll promises
  | not inBrowser = pure (map (const (Left notInBrowser)) promises)
  | otherwise = liftIO $ do
      outcomes <- js_outcomes
      each <- traverse (\p -> js_outcome p >>= \o -> o <$ js_push outcomes o) promises
      settle (js_settle_all outcomes)
      traverse outcomeOf each

-- An awaited import on WebAssembly returns a thunk, and the thread waits for
-- the promise only when it is forced; forcing it here means no caller has
-- to. On the JavaScript backend the import is interruptible and has waited
-- by the time it returns.
settle :: IO () -> IO ()
settle wait = wait >>= evaluate

outcomeOf :: JSVal -> IO (Either JSError JSVal)
outcomeOf outcome = do
  ok <- js_ok outcome
  if ok
    then Right <$> js_value outcome
    else do
      name <- fromJSText <$> js_name outcome
      message <- fromJSText <$> js_message outcome
      pure (Left JSError {name, message})

notInBrowser :: JSError
notInBrowser = JSError {name = "NotInBrowserError", message = "there is no JavaScript engine here to run a promise"}
