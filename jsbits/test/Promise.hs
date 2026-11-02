{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Waiting for promises that settle later: under Node on
-- both browser backends, and natively, where there is nothing to wait for.
module Main (main) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad (unless)
import Data.Text (Text)
import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, inBrowser, jsValText, toJSText)
import Halogen.JSBits.Promise (JSError (..), Promise (..), mapConcurrently, race, waitAll, waitAnyCatch, waitCatch)
import System.IO (BufferMode (..), hSetBuffering, stdout)

$( browserJS
     ["test/promise.js"]
     [ ("text", "halogen_promise_test_text", Safe, [t|Int -> JSText -> IO (Promise JSVal)|])
     , ("number", "halogen_promise_test_number", Safe, [t|Int -> Int -> IO (Promise JSVal)|])
     , ("failing", "halogen_promise_test_error", Safe, [t|Int -> JSText -> JSText -> IO (Promise JSVal)|])
     , ("rejectWithText", "halogen_promise_test_reject_with", Safe, [t|Int -> JSText -> IO (Promise JSVal)|])
     , ("rejectWithNumber", "halogen_promise_test_reject_with", Safe, [t|Int -> Int -> IO (Promise JSVal)|])
     , ("plain", "halogen_promise_test_plain", Safe, [t|JSText -> IO (Promise JSVal)|])
     , ("typedText", "halogen_promise_test_text", Safe, [t|Int -> JSText -> IO (Promise Text)|])
     , ("typedNumber", "halogen_promise_test_number", Safe, [t|Int -> Int -> IO (Promise Int)|])
     , ("typedNumbers", "halogen_promise_test_numbers", Safe, [t|Int -> IO (Promise [Int])|])
     , ("readNumber", "halogen_promise_test_read_number", Safe, [t|JSVal -> IO Int|])
     , ("isString", "halogen_promise_test_is_string", Safe, [t|JSVal -> IO Bool|])
     ]
 )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  if inBrowser then inEngine else natively
  putStrLn "Promise checks passed"

inEngine :: IO ()
inEngine = do
  resolved <- waitCatch =<< text 20 (toJSText "done")
  check "a resolved promise gives its value" =<< isText "done" resolved
  checkNumber "a number comes back as it went" 42 =<< (waitCatch =<< number 10 42)
  typed <- waitCatch =<< typedText 10 (toJSText "typed")
  check "a Promise Text resolves with its text" (typed == Right "typed")
  typedN <- waitCatch =<< typedNumber 10 41
  check "a Promise Int resolves with its number" (typedN == Right 41)
  earliest <- waitAnyCatch =<< sequence [typedNumber 40 1, typedNumber 10 2]
  check "waitAnyCatch gives the promise that settles first" (snd earliest == Right 2)
  check "mapConcurrently gives each result in order" . (== [3, 1, 2]) =<< mapConcurrently (\n -> typedNumber (10 * n) n) [3, 1, 2]
  check "race gives the first to settle" . (== Right 2) =<< race (typedNumber 40 1) (typedNumber 10 2)
  early <- try (waitAll =<< sequence [number 200 1, failing 10 (toJSText "TypeError") (toJSText "early")])
  check "waitAll throws a rejection as soon as it comes" (either (\(e :: JSError) -> e.name == "TypeError") (const False) (early :: Either JSError [JSVal]))
  typedList <- waitCatch =<< typedNumbers 10
  check "a Promise [Int] resolves with its numbers" (typedList == Right [1, 2, 3])

  failed <- waitCatch =<< failing 10 (toJSText "NotFoundError") (toJSText "no such thing")
  check "an Error's rejection gives its name and message" (failed `isError` JSError "NotFoundError" "no such thing")
  rejectedText <- waitCatch =<< rejectWithText 10 (toJSText "nope")
  check "a string's rejection is an Error with the string as its message" (rejectedText `isError` JSError "Error" "nope")
  rejectedNumber <- waitCatch =<< rejectWithNumber 10 7
  check "a number's rejection is an Error with the number as its message" (rejectedNumber `isError` JSError "Error" "7")

  check "a value that is no promise stands for one resolved with it" =<< (isText "plain" =<< (waitCatch =<< plain (toJSText "plain")))

  -- Started in this order, settling in the other.
  outcomes <-
    traverse waitCatch
      =<< sequence [number 30 1, failing 20 (toJSText "TypeError") (toJSText "second"), text 10 (toJSText "third")]
  case outcomes of
    [first, second, third] -> do
      checkNumber "waitCatch on each gives the first outcome first" 1 first
      check "waitCatch on each gives the second outcome second" (second `isError` JSError "TypeError" "second")
      check "waitCatch on each gives the third outcome third" =<< isText "third" third
    _ -> failure ("waitCatch on each gives one outcome for each promise, not " <> show (length outcomes))
  check "waitCatch on nothing gives nothing" . null =<< traverse waitCatch ([] :: [Promise JSVal])

  shared <- number 20 5
  checkNumber "the first waitCatch of a promise gets its value" 5 =<< waitCatch shared
  checkNumber "a second waitCatch of the same promise, once it has settled, gets it too" 5 =<< waitCatch shared
  concurrent <- number 20 7
  box <- newEmptyMVar
  _ <- forkIO (waitCatch concurrent >>= putMVar box)
  checkNumber "a thread awaiting a promise gets its value" 7 =<< waitCatch concurrent
  checkNumber "another thread awaiting the same promise at the same time gets it too" 7 =<< takeMVar box
  again <- number 20 6
  traverse waitCatch [again, again] >>= \case
    [one, two] -> do
      checkNumber "awaiting the same promise twice at once gives it to the first" 6 one
      checkNumber "awaiting the same promise twice at once gives it to the second" 6 two
    _ -> failure "waitCatch on each gives one outcome for each promise"
  where
    isText expected = \case
      Right value -> (\s -> s && jsValText value == expected) <$> isString value
      Left _ -> pure False
    checkNumber message expected = \case
      Right value -> check message . (== expected) =<< readNumber value
      Left e -> failure (message <> ": rejected with " <> show e)

natively :: IO ()
natively = do
  outcome <- waitCatch =<< text 0 (toJSText "done")
  check "natively waitCatch gives NotInBrowserError" (outcome `isError` JSError "NotInBrowserError" "")
  outcomes <- traverse waitCatch =<< sequence [number 0 1, number 0 2]
  check "natively waitCatch on each gives NotInBrowserError for each" (length outcomes == 2 && all (`isError` JSError "NotInBrowserError" "") outcomes)

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
