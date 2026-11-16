{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The protocol between a page and its workers, apart from the browser:
-- what a 'Job' holds, the bytes of a request and of a reply, how a worker
-- answers a request, and how "Halogen.Worker.Par" cuts the calls it has
-- collected into batches. "Halogen.Worker" is the interface; this module
-- is exposed so that the protocol can be tested without a worker (the
-- package's own tests make up requests no page would send), and it may
-- change in any version.
module Halogen.Worker.Internal
  ( -- * Jobs
    Job (..)
  , job
  , curried
  , jobName
  , BadArguments (..)

    -- * A job's function
  , Fn
  , JobFunction (..)
  , JobArgument (..)
  , ArgsOf
  , ResultOf
  , Lifted

    -- * Requests and replies
  , Request (..)
  , encodeRequest
  , Reply (..)
  , decodeReply
  , decodeStrict
  , decodeStrictWith

    -- * Answering
  , answer
  , runCall

    -- * Batches
  , Batch (..)
  , plan
  , batchesPerWorker
  )
where

import Control.Exception (Exception (..), SomeException (..), evaluate, throwIO)
import Data.Binary (Binary (..), Get, Put, encode)
import Data.Binary.Get (runGetOrFail)
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import GHC.Exts (FUN, LiftedRep, Multiplicity (..), TYPE)
import GHC.StaticPtr (StaticKey, StaticPtr, StaticPtrInfo (..), deRefStaticPtr, staticPtrInfo, unsafeLookupStaticPtr)
import GHC.TypeError (ErrorMessage (..), Unsatisfiable)
import UnliftIO.Exception (tryAny)

-- | A function a worker can run, of the arguments @args@, in order, to a
-- @b@, already wrapped in the codecs of its arguments and result, so that
-- whoever holds a 'Job' needs no instance of its own to send it: the
-- 'Data.Binary.Binary' dictionaries are inside it, and so inside the static
-- value a @static@ makes of it.
data Job (args :: [Type]) b = Job
  { call :: ByteString -> IO ByteString
  -- ^ The function as a worker runs it, from its arguments' bytes to its
  -- result's, throwing what the function throws. Its type mentions neither
  -- @args@ nor @b@, which is what lets a worker run a job it found by its
  -- key alone (see 'answer').
  , withArguments :: forall r. (ByteString -> r) -> Fn args r
  -- ^ The arguments taken one by one, as the function takes them, and
  -- their bytes handed on: what a call of the job sends. Read it with
  -- 'curried', since a field of this type has no 'GHC.Records.HasField'.
  , decodeResult :: ByteString -> Either String b
  }

-- | The function of these arguments, in order, to @r@:
-- @Fn '[Int, Text] r@ is @Int -> Text -> r@, and @Fn '[] r@ is @r@.
type family Fn (args :: [Type]) (r :: Type) :: Type where
  Fn '[] r = r
  Fn (a ': as) r = a -> Fn as r

-- | The arguments of a function given to 'job', which ends in 'IO'.
type family ArgsOf (f :: Type) :: [Type] where
  ArgsOf (FUN w (a :: TYPE r) f) = Lifted a ': ArgsOf f
  ArgsOf (IO b) = '[]

-- | The result of a function given to 'job', in its 'IO'.
type family ResultOf (f :: Type) :: Type where
  ResultOf (FUN w (a :: TYPE r) f) = ResultOf f
  ResultOf (IO b) = b

-- | An argument's type as an ordinary type, which it is once its
-- representation is known to be lifted (see 'JobFunction').
type Lifted :: forall r. TYPE r -> Type
type family Lifted a where
  Lifted @LiftedRep a = a

-- | A function a job can be made of: curried, of 'Binary' arguments, ending
-- in 'IO' of a 'Binary' result. The instances go by the function's type,
-- one argument at a time, and so take any number of arguments.
--
-- The instance for an arrow is incoherent so that a function whose monad
-- is not known yet, such as @\\n -> pure (n + 1)@, of type
-- @Int -> m0 Int@, is still taken apart: at @m0 Int@ the arrow's instance
-- does not match (it would only once @m0@ were @(->) x@), and GHC may choose
-- the other instance when every instance that does not match but might is
-- incoherent; that instance then makes the monad 'IO'. At an arrow both
-- instances match, and the arrow's, the more specific, is chosen.
--
-- The arrow's instance must match every arrow a lambda can have when the
-- instance is chosen, or the other instance would be chosen, and fail, in
-- its place. So its argument may be of any representation and its arrow of
-- any multiplicity, both made the ordinary ones by the instance's context:
-- the type of an argument nothing else constrains (@\\_ -> ...@) has a
-- representation that is still a variable then, and a lambda whose
-- argument is matched against a constructor (@\\() -> ...@) has an arrow
-- whose multiplicity is still a variable.
--
-- A last instance, overlappable, takes whatever is neither, to say what is
-- wrong in a message of its own rather than as a missing instance.
class JobFunction f where
  -- | Collect the arguments one by one, each one's 'put' after those
  -- before, and hand the whole on.
  taking :: Proxy f -> Put -> (Put -> x) -> Fn (ArgsOf f) x

  -- | Read the arguments one by one, in the same order, and apply the
  -- function to them.
  giving :: f -> Get (IO (ResultOf f))

instance {-# INCOHERENT #-} (w ~ 'Many, r ~ LiftedRep, JobArgument a, JobFunction f) => JobFunction (FUN w (a :: TYPE r) f) where
  taking _ before k = takingOne (Proxy @a) before (\after -> taking (Proxy @f) after k)
  giving function = givingOne function >>= giving

instance (m ~ IO) => JobFunction (m b) where
  taking _ before k = k before
  giving action = pure action

-- | Anything else, which is not a function ending in 'IO', is a mistake.
instance
  {-# OVERLAPPABLE #-}
  ( Unsatisfiable
      ( 'Text "A job is made of a function whose result is in IO, such as Int -> Text -> IO Int,"
          ':$$: 'Text "and it ends in "
          ':<>: 'ShowType f
          ':<>: 'Text " here."
          ':$$: 'Text "A pure result is in IO with pure: \\n -> pure (f n)."
      )
  )
  => JobFunction f

-- | One argument of a 'JobFunction', as a class of its own so that the
-- arrow's instance can name an argument of any representation: its only
-- instance is for lifted types, of 'Binary'.
class JobArgument (a :: TYPE r) where
  -- | The argument's 'put' after those before, and the whole handed on.
  takingOne :: Proxy a -> Put -> (Put -> x) -> a -> x

  -- | The function applied to the argument read.
  givingOne :: (a -> f) -> Get f

instance (Binary a) => JobArgument (a :: Type) where
  takingOne _ before k argument = k (before >> put argument)
  givingOne function = function <$> get

-- | The job of a function, which takes its arguments one by one, as any
-- function does, and gives its result in 'IO': see "Halogen.Worker"'s
-- 'Halogen.Worker.job'. The arguments' bytes are their 'put's one after
-- another, which for one argument is what 'encode' gives.
job :: forall f. (JobFunction f, Binary (ResultOf f)) => f -> Job (ArgsOf f) (ResultOf f)
job function =
  Job
    { call = \bytes -> case decodeStrictWith (giving function) bytes of
        Left problem -> throwIO (BadArguments problem)
        Right action -> Lazy.toStrict . encode <$> action
    , withArguments = \k -> taking (Proxy @f) (pure ()) (k . Lazy.toStrict . runPut)
    , decodeResult = decodeStrict
    }

-- | What a call of the job does with its arguments, taken one by one: their
-- bytes handed to the continuation.
curried :: Job args b -> (ByteString -> r) -> Fn args r
curried Job {withArguments} = withArguments

-- | What a job whose arguments' bytes do not decode throws: bytes that run
-- out, or bytes left over, which mean the page and the worker disagree
-- about the arguments' types.
newtype BadArguments = BadArguments String
  deriving stock (Show)

instance Exception BadArguments where
  displayException (BadArguments problem) = "its arguments do not decode: " <> problem

-- | The job's name in a 'Halogen.Worker.WorkerError': where its @static@ is,
-- the module and the line and column, which identify it as well as its key
-- does and say more to a reader.
jobName :: StaticPtr (Job args b) -> Text
jobName pointer =
  let info = staticPtrInfo pointer
      (line, column) = info.spInfoSrcLoc
   in Text.pack (info.spInfoModuleName <> ":" <> show line <> ":" <> show column)

-- | What a page asks of a worker: the job, by the key of its static
-- pointer, which is the same in every copy of the program, and the bytes of
-- its calls' arguments, one entry for each call (all of a call's arguments
-- in one entry, see 'withArguments'). A list, so that one request carries a
-- batch of calls of the same job ("Halogen.Worker.Par"'s);
-- 'Halogen.Worker.start' sends one.
data Request = Request
  { key :: StaticKey
  , arguments :: [ByteString]
  }

instance Binary Request where
  put request = put request.key >> put request.arguments
  get = Request <$> get <*> get

encodeRequest :: Request -> ByteString
encodeRequest = Lazy.toStrict . encode

-- | A worker's answer to a request it could run: the results of its calls,
-- in order, up to the first that failed, and that one's failure. The calls
-- after a failure are not made, since the batch has failed whatever they
-- give; and the failing call's position is the number of results before it,
-- which is how the page names it.
data Reply = Reply
  { results :: [ByteString]
  , failure :: Maybe Text
  }
  deriving stock (Eq, Show)

instance Binary Reply where
  put reply = put reply.results >> put (encodeUtf8 <$> reply.failure)
  get = Reply <$> get <*> (fmap decodeUtf8Lenient <$> get)

decodeReply :: ByteString -> Either String Reply
decodeReply = decodeStrict

-- | Bytes as a value of the type, all of them: bytes left over mean the
-- two sides disagree about the type, which is as much an error as bytes
-- that run out.
decodeStrict :: (Binary a) => ByteString -> Either String a
decodeStrict = decodeStrictWith get

-- | 'decodeStrict' with the decoder given.
decodeStrictWith :: Get a -> ByteString -> Either String a
decodeStrictWith decoder bytes = case runGetOrFail decoder (Lazy.fromStrict bytes) of
  Left (_, _, problem) -> Left problem
  Right (rest, offset, value)
    | Lazy.null rest -> Right value
    | otherwise -> Left (show (Lazy.length rest) <> " bytes are left over after the first " <> show offset)

-- | A worker's answer to a request's bytes: the 'Reply''s bytes, or what
-- went wrong with the request as a whole (it does not decode, or names a
-- job the program does not have).
--
-- The job is found in the program's static pointer table by its key.
-- 'unsafeLookupStaticPtr' gives a pointer of whatever type it is asked for,
-- here @'StaticPtr' ('Job' '[] ())@: every key a page sends was taken from
-- a @'StaticPtr' ('Job' args b)@ for some @args@ and @b@, so what the key
-- names is a 'Job', and the only field read is 'call', whose type does not
-- depend on @args@ or @b@. A key the table does not have (a page and a worker that are
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
      Just (pointer :: StaticPtr (Job '[] ())) -> do
        let function = (deRefStaticPtr pointer).call
            go done [] = pure Reply {results = reverse done, failure = Nothing}
            go done (argument : rest) =
              runCall function argument >>= \case
                Left problem -> pure Reply {results = reverse done, failure = Just problem}
                Right result -> go (result : done) rest
        Right . Lazy.toStrict . encode <$> go [] request.arguments

-- | Run a job's function on its arguments' bytes, giving its result's bytes,
-- forced, or its exception's message. Only the function's synchronous
-- exceptions are caught; one thrown to the thread (it was killed) goes on.
runCall :: (ByteString -> IO ByteString) -> ByteString -> IO (Either Text ByteString)
runCall function argument = do
  outcome <- tryAny (function argument >>= evaluate)
  pure $ case outcome of
    Left (SomeException e) -> Left (Text.pack (displayException e))
    Right result -> Right result

-- | Calls of one job, by the job's key, sent to a worker as one request.
data Batch k = Batch
  { key :: k
  , offset :: Int
  -- ^ The position, among the job's calls, of the batch's first: the
  -- index a failure in the batch is named by is this plus the failing
  -- call's place in it.
  , positions :: [Int]
  -- ^ Where the batch's calls are in the list 'plan' was given, in order,
  -- which is where their results go back to.
  }
  deriving stock (Eq, Show)

-- | How many batches a job's calls are cut into for each worker: more than
-- one, so that a worker that finishes early takes another batch while a
-- slower one is still at its own, rather than wait for it with nothing to
-- do; few enough that each batch is worth its request.
batchesPerWorker :: Int
batchesPerWorker = 4

-- | The batches for calls of these keys (one per call, in the order they
-- were collected), for a pool of this many workers: the calls grouped by
-- key, the keys in the order they first appear, each group cut in order
-- into at most 'batchesPerWorker' batches per worker of sizes that differ
-- by one at most, the larger first. The batches come in that order, a
-- key's after the one before's, which is the order the workers are handed
-- them in.
plan :: (Ord k) => Int -> [k] -> [Batch k]
plan workers keys = concatMap cut grouped
  where
    -- The keys in the order they first appear, each with its calls'
    -- positions, in order.
    grouped =
      let (order, groups) = foldl' collect ([], Map.empty) (zip [0 ..] keys)
       in [(k, reverse (groups Map.! k)) | k <- reverse order]
    collect (order, groups) (position, k) =
      ( if Map.member k groups then order else k : order
      , Map.insertWith (<>) k [position] groups
      )
    cut (k, positions) =
      let n = length positions
          count = min n (max 1 workers * batchesPerWorker)
          sizes = [n `div` count + (if i < n `mod` count then 1 else 0) | i <- [0 .. count - 1]]
          offsets = scanl (+) 0 sizes
       in [Batch {key = k, offset, positions = take size (drop offset positions)} | (offset, size) <- zip offsets sizes]
