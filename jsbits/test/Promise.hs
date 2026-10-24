{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 'await' and 'awaitAll' against promises that settle later: under Node on
-- both browser backends, and natively, where there is nothing to await.
module Main (main) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless)
import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, inBrowser, jsValText, toJSText)
import Halogen.JSBits.Promise (JSError (..), Promise (..), await, awaitAll)
import System.IO (BufferMode (..), hSetBuffering, stdout)

$( browserJS
     ["test/promise.js"]
     [ ("text", "halogen_promise_test_text", Unsafe, [t|Int -> JSText -> IO Promise|])
     , ("number", "halogen_promise_test_number", Unsafe, [t|Int -> Int -> IO Promise|])
     , ("failing", "halogen_promise_test_error", Unsafe, [t|Int -> JSText -> JSText -> IO Promise|])
     , ("rejectWithText", "halogen_promise_test_reject_with", Unsafe, [t|Int -> JSText -> IO Promise|])
     , ("rejectWithNumber", "halogen_promise_test_reject_with", Unsafe, [t|Int -> Int -> IO Promise|])
     , ("plain", "halogen_promise_test_plain", Unsafe, [t|JSText -> IO Promise|])
     , ("readNumber", "halogen_promise_test_read_number", Unsafe, [t|JSVal -> IO Int|])
     , ("isString", "halogen_promise_test_is_string", Unsafe, [t|JSVal -> IO Bool|])
     ]
 )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  if inBrowser then inEngine else natively
  putStrLn "Promise checks passed"

inEngine :: IO ()
inEngine = do
  resolved <- await =<< text 20 (toJSText "done")
  check "a resolved promise gives its value" =<< isText "done" resolved
  checkNumber "a number comes back as it went" 42 =<< (await =<< number 10 42)

  failed <- await =<< failing 10 (toJSText "NotFoundError") (toJSText "no such thing")
  check "an Error's rejection gives its name and message" (failed `isError` JSError "NotFoundError" "no such thing")
  rejectedText <- await =<< rejectWithText 10 (toJSText "nope")
  check "a string's rejection is an Error with the string as its message" (rejectedText `isError` JSError "Error" "nope")
  rejectedNumber <- await =<< rejectWithNumber 10 7
  check "a number's rejection is an Error with the number as its message" (rejectedNumber `isError` JSError "Error" "7")

  check "a value that is no promise stands for one resolved with it" =<< (isText "plain" =<< (await =<< plain (toJSText "plain")))

  -- Started in this order, settling in the other.
  outcomes <-
    awaitAll
      =<< sequence [number 30 1, failing 20 (toJSText "TypeError") (toJSText "second"), text 10 (toJSText "third")]
  case outcomes of
    [first, second, third] -> do
      checkNumber "awaitAll gives the first outcome first" 1 first
      check "awaitAll gives the second outcome second" (second `isError` JSError "TypeError" "second")
      check "awaitAll gives the third outcome third" =<< isText "third" third
    _ -> failure ("awaitAll gives one outcome for each promise, not " <> show (length outcomes))
  check "awaitAll of nothing gives nothing" . null =<< awaitAll []

  shared <- number 20 5
  checkNumber "the first await of a promise gets its value" 5 =<< await shared
  checkNumber "a second await of the same promise, once it has settled, gets it too" 5 =<< await shared
  concurrent <- number 20 7
  box <- newEmptyMVar
  _ <- forkIO (await concurrent >>= putMVar box)
  checkNumber "a thread awaiting a promise gets its value" 7 =<< await concurrent
  checkNumber "another thread awaiting the same promise at the same time gets it too" 7 =<< takeMVar box
  again <- number 20 6
  awaitAll [again, again] >>= \case
    [one, two] -> do
      checkNumber "awaiting the same promise twice at once gives it to the first" 6 one
      checkNumber "awaiting the same promise twice at once gives it to the second" 6 two
    _ -> failure "awaitAll gives one outcome for each promise"
  where
    isText expected = \case
      Right value -> (\s -> s && jsValText value == expected) <$> isString value
      Left _ -> pure False
    checkNumber message expected = \case
      Right value -> check message . (== expected) =<< readNumber value
      Left e -> failure (message <> ": rejected with " <> show e)

natively :: IO ()
natively = do
  outcome <- await =<< text 0 (toJSText "done")
  check "natively await gives NotInBrowserError" (outcome `isError` JSError "NotInBrowserError" "")
  outcomes <- awaitAll =<< sequence [number 0 1, number 0 2]
  check "natively awaitAll gives NotInBrowserError for each" (length outcomes == 2 && all (`isError` JSError "NotInBrowserError" "") outcomes)

-- | Whether the outcome is this rejection; natively only the name is
-- compared, its message being the package's own sentence.
isError :: Either JSError JSVal -> JSError -> Bool
isError (Left (JSError name message)) (JSError expectedName expectedMessage) =
  name == expectedName && (message == expectedMessage || not inBrowser)
isError (Right _) _ = False

check :: String -> Bool -> IO ()
check message result = unless result (failure message)

failure :: String -> IO a
failure message = ioError (userError message)

#if defined(wasm32_HOST_ARCH)
-- On WebAssembly a test that waits for promises is a reactor: a command's
-- main cannot wait for one, so wasm-test-runner.mjs calls this export and
-- waits for the promise it returns.
foreign export javascript "hs_test" main :: IO ()
#endif
