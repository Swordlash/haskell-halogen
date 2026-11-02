-- | The protocol between a page and its workers, apart from the browser:
-- what a 'Job' holds, the bytes of a request and of a reply, and how a
-- worker answers a request. "Halogen.Worker" is the interface; this module
-- is exposed so that the protocol can be tested without a worker (the
-- package's own tests make up requests no page would send), and it may
-- change in any version.
module Halogen.Worker.Internal
  ( -- * Jobs
    Job (..)
  , jobName

    -- * Requests and replies
  , Request (..)
  , encodeRequest
  , decodeReply
  , decodeStrict

    -- * Answering
  , answer
  , runCall
  )
where

import Control.Exception (Exception (..), SomeException (..), evaluate)
import Data.Binary (Binary (..), decodeOrFail, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.StaticPtr (StaticKey, StaticPtr, StaticPtrInfo (..), deRefStaticPtr, staticPtrInfo, unsafeLookupStaticPtr)
import UnliftIO.Exception (tryAny)

-- | A function a worker can run, from @a@ to @b@, already wrapped in the
-- codecs of its argument and result, so that whoever holds a 'Job' needs no
-- instance of its own to send it: the 'Data.Binary.Binary' dictionaries are
-- inside it, and so inside the static value a @static@ makes of it.
data Job a b = Job
  { call :: ByteString -> IO ByteString
  -- ^ The function as a worker runs it, from its argument's bytes to its
  -- result's, throwing what the function throws. Its type mentions neither
  -- @a@ nor @b@, which is what lets a worker run a job it found by its key
  -- alone (see 'answer').
  , encodeArgument :: a -> ByteString
  , decodeResult :: ByteString -> Either String b
  }

-- | The job's name in a 'Halogen.Worker.WorkerError': where its @static@ is,
-- the module and the line and column, which identify it as well as its key
-- does and say more to a reader.
jobName :: StaticPtr (Job a b) -> Text
jobName pointer =
  let info = staticPtrInfo pointer
      (line, column) = info.spInfoSrcLoc
   in Text.pack (info.spInfoModuleName <> ":" <> show line <> ":" <> show column)

-- | What a page asks of a worker: the job, by the key of its static
-- pointer, which is the same in every copy of the program, and its
-- arguments' bytes. A list, so that one request can carry a batch of calls
-- of the same job; the page sends one argument for now.
data Request = Request
  { key :: StaticKey
  , arguments :: [ByteString]
  }

instance Binary Request where
  put request = put request.key >> put request.arguments
  get = Request <$> get <*> get

encodeRequest :: Request -> ByteString
encodeRequest = Lazy.toStrict . encode

-- | A reply's results, one for each of its request's arguments, in order.
decodeReply :: ByteString -> Either String [ByteString]
decodeReply = decodeStrict

-- | Bytes as a value of the type, all of them: bytes left over mean the
-- two sides disagree about the type, which is as much an error as bytes
-- that run out.
decodeStrict :: (Binary a) => ByteString -> Either String a
decodeStrict bytes = case decodeOrFail (Lazy.fromStrict bytes) of
  Left (_, _, problem) -> Left problem
  Right (rest, offset, value)
    | Lazy.null rest -> Right value
    | otherwise -> Left (show (Lazy.length rest) <> " bytes are left over after the first " <> show offset)

-- | A worker's answer to a request's bytes: the reply's bytes, or what went
-- wrong, which fails the whole request.
--
-- The job is found in the program's static pointer table by its key.
-- 'unsafeLookupStaticPtr' gives a pointer of whatever type it is asked for,
-- here @'StaticPtr' ('Job' () ())@: every key a page sends was taken from a
-- @'StaticPtr' ('Job' a b)@ for some @a@ and @b@, so what the key names is a
-- 'Job', and the only field read is 'call', whose type does not depend on
-- @a@ or @b@. A key the table does not have (a page and a worker that are
-- not the same binary) is the request's error, with the key in it.
answer :: ByteString -> IO (Either Text ByteString)
answer bytes = case decodeStrict @Request bytes of
  Left problem -> pure (Left ("the request does not decode: " <> Text.pack problem))
  Right request ->
    unsafeLookupStaticPtr request.key >>= \case
      Nothing ->
        pure . Left $
          "there is no job with the key "
            <> Text.pack (show request.key)
            <> " in this program's static pointer table: the worker is not a copy of the page's program"
      Just (pointer :: StaticPtr (Job () ())) -> do
        let function = (deRefStaticPtr pointer).call
        results <- traverse (runCall function) request.arguments
        pure (Lazy.toStrict . encode <$> sequence results)

-- | Run a job's function on its argument's bytes, giving its result's bytes,
-- forced, or its exception's message. Only the function's synchronous
-- exceptions are caught; one thrown to the thread (it was killed) goes on.
runCall :: (ByteString -> IO ByteString) -> ByteString -> IO (Either Text ByteString)
runCall function argument = do
  outcome <- tryAny (function argument >>= evaluate)
  pure $ case outcome of
    Left (SomeException e) -> Left (Text.pack (displayException e))
    Right result -> Right result
