-- | Protolude adapted to UnliftIO
module HPrelude
  ( -- * Base functions
    module Base
  , identity
  , pass

    -- * Function functions
  , module Function
  , applyN

    -- * List functions
  , module List
  , map
  , uncons
  , unsnoc

    -- * Data Structures
  , module DataStructures

    -- * Show functions
  , module Show
  , show
  , print

    -- * Bool functions
  , module Bool

    -- * Monad functions
  , module Monad
  , liftIO1
  , liftIO2

    -- * Functor functions
  , module Functor

    -- * Either functions
  , module Either

    -- * Applicative functions
  , module Applicative
  , guarded
  , guardedA

    -- * String conversion
  , module ConvertText

    -- * Debug functions
  , module Debug

    -- * Panic functions
  , module Panic

    -- * Exception functions
  , module Exception
  , throwIO
  , throwTo

    -- * Semiring functions
  , module Semiring

    -- * String functions
  , module String

    -- * Safe functions
  , module Safe

    -- * Eq functions
  , module Eq

    -- * Ord functions
  , module Ord

    -- * Traversable functions
  , module Traversable

    -- * Foldable functions
  , module Foldable

    -- * Semigroup functions
  , module Semigroup

    -- * Monoid functions
  , module Monoid

    -- * Bifunctor functions
  , module Bifunctor

    -- * Bifunctor functions
  , module Hashable

    -- * Deepseq functions
  , module DeepSeq

    -- * Tuple functions
  , module Tuple
  , module Typeable

    -- * Typelevel programming
  , module Typelevel

    -- * Monads
  , module Fail
  , module State
  , module Reader
  , module Except
  , module Trans
  , module ST
  , module STM

    -- * Integers
  , module Int
  , module Bits

    -- * Complex functions
  , module Complex

    -- * Char functions
  , module Char

    -- * Maybe functions
  , module Maybe

    -- * Generics functions
  , module Generics

    -- * ByteString functions
  , module ByteString
  , LByteString

    -- * Text functions
  , module Text
  , LText

    -- * Read functions
  , module Read
  , readMaybe
  , readEither

    -- * System functions
  , module System
  , die

    -- * Concurrency functions
  , module Concurrency

    -- * Foreign functions
  , module Foreign
  , atomicModifyIORef'_
  , UnliftIO (..)
  , askUnliftIO
  , mapUnliftIO
  , MonadUnliftIO (..)
  , module Coercible
  , loopM
  , MonadMask

  , unsafeCoerce
  )
where

-- Protolude module exports.

-- Overriden by Show.putStr
-- Overriden by Show.putStrLn
-- Overriden by Protolude.print
-- Overriden by Protolude.show
-- Custom Show instances deprecated.
-- Custom Show instances deprecated.
-- Custom Show instances deprecated.
-- Custom Show instances deprecated.
-- Custom Show instances deprecated.

-- Used for 'show', not exported.

-- Maybe'ized version of partial functions

-- Applicatives
import Control.Applicative as Applicative
  ( Alternative (..)
  , Applicative (..)
  , Const (Const, getConst)
  , ZipList (ZipList, getZipList)
  , liftA
  , liftA2
  , liftA3
  , optional
  , (<**>)
  )
import Data.Coerce as Coercible (Coercible, coerce)
-- Base typeclasses
import Data.Eq as Eq
  ( Eq (..)
  )
import Data.Foldable as Foldable
  ( Foldable
  , all
  , and
  , any
  , asum
  , concat
  , concatMap
  , elem
  , find
  , fold
  , foldMap
  , foldl
  , foldl'
  , foldlM
  , foldr
  , foldr'
  , foldrM
  , forM_
  , for_
  , length
  , mapM_
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , msum
  , notElem
  , null
  , or
  , sequenceA_
  , sequence_
  , toList
  , traverse_
  )
import Data.Functor.Identity as Functor
  ( Identity (Identity, runIdentity)
  )
import Data.List.NonEmpty as List
  ( NonEmpty ((:|))
  , nonEmpty
  )
import Data.Ord as Ord
  ( Down (Down)
  , Ord (..)
  , Ordering (EQ, GT, LT)
  , comparing
  )
import Data.Semigroup as Semigroup
  ( Semigroup (sconcat, stimes)
  , WrappedMonoid
  , cycle1
  , diff
  , mtimesDefault
  , stimesIdempotent
  , stimesIdempotentMonoid
  , stimesMonoid
  )
import Data.String (String)
import Data.String as String (IsString)
import Data.Traversable as Traversable
import Protolude.Applicative as Applicative
import Protolude.Base as Base hiding
  ( print
  , putStr
  , putStrLn
  , show
  , showFloat
  , showList
  , showSigned
  , showSignedFloat
  , showsPrec
  )
import Protolude.Base qualified as PBase
import Protolude.Bool as Bool
import Protolude.Conv qualified as Conv
import Protolude.ConvertText as ConvertText
import Protolude.Debug as Debug
import Protolude.Either as Either
import Protolude.Exceptions as Exception
import Protolude.Functor as Functor
import Protolude.List as List
import Protolude.Monad as Monad
import Protolude.Panic as Panic
import Protolude.Safe as Safe
  ( atDef
  , atMay
  , foldl1May
  , foldl1May'
  , foldr1May
  , headDef
  , headMay
  , initDef
  , initMay
  , initSafe
  , lastDef
  , lastMay
  , maximumDef
  , maximumMay
  , minimumDef
  , minimumMay
  , tailDef
  , tailMay
  , tailSafe
  )
import Protolude.Semiring as Semiring
import Protolude.Show as Show
import UnliftIO as UnliftIO

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,16,0)
import Data.Semigroup as Semigroup (
    Option(..)
  , option
  )
#endif

-- Deepseq
import Control.DeepSeq as DeepSeq
  ( NFData (..)
  , deepseq
  , force
  , ($!!)
  )
-- Data structures

-- Hashing

-- Monad transformers

-- Base types

-- Genericss

-- ByteString

-- Text

-- IO

-- ST

-- Concurrency and Parallelism
import Control.Exception as Exception
  ( AllocationLimitExceeded (AllocationLimitExceeded)
  , ArithException
    ( Denormal
    , DivideByZero
    , LossOfPrecision
    , Overflow
    , RatioZeroDenominator
    , Underflow
    )
  , ArrayException (IndexOutOfBounds, UndefinedElement)
  , AssertionFailed (AssertionFailed)
  , AsyncException (HeapOverflow, StackOverflow, ThreadKilled, UserInterrupt)
  , BlockedIndefinitelyOnMVar (BlockedIndefinitelyOnMVar)
  , BlockedIndefinitelyOnSTM (BlockedIndefinitelyOnSTM)
  , CompactionFailed (CompactionFailed)
  , Deadlock (Deadlock)
  , ErrorCall (ErrorCall, ErrorCallWithLocation)
  , MaskingState (..)
  , NestedAtomically (NestedAtomically)
  , NoMethodError (NoMethodError)
  , NonTermination (NonTermination)
  , PatternMatchFail (PatternMatchFail)
  , RecConError (RecConError)
  , RecSelError (RecSelError)
  , RecUpdError (RecUpdError)
  , TypeError (TypeError)
  , allowInterrupt
  , getMaskingState
  , interruptible
  , ioError
  , mapException
  )
-- , Handler(Handler)

-- , addMVarFinalizer

-- Read instances hiding unsafe builtins (read)

import Control.Exception.Safe (MonadMask (..))
import Control.Monad.Except as Except
  ( Except
  , ExceptT (ExceptT)
  , MonadError
  , catchError
  , mapExcept
  , mapExceptT
  , runExcept
  , runExceptT
  , throwError
  , withExcept
  , withExceptT
  )
import Control.Monad.Fail as Fail
  ( MonadFail
  )
import Control.Monad.Reader as Reader
  ( MonadReader
  , Reader
  , ReaderT (ReaderT)
  , ask
  , asks
  , local
  , reader
  , runReader
  , runReaderT
  )
import Control.Monad.ST as ST
  ( ST
  , fixST
  , runST
  )
import Control.Monad.STM as STM
  ( STM
  , atomically
  , catchSTM
  , check
  , orElse
  , retry
  , throwSTM
  )
import Control.Monad.State as State
  ( MonadState
  , State
  , StateT (StateT)
  , evalState
  , evalStateT
  , execState
  , execStateT
  , get
  , gets
  , modify
  , put
  , runState
  , runStateT
  , state
  , withState
  )
import Control.Monad.Trans as Trans
  ( MonadIO
  , MonadTrans (..)
  , lift
  , liftIO
  )
import Control.Monad.Trans.Except as Except
  ( catchE
  , throwE
  )
import Data.Bifunctor as Bifunctor (Bifunctor (bimap, first, second))
import Data.Bits as Bits
  ( Bits
  , FiniteBits
  , bit
  , bitDefault
  , bitSize
  , bitSizeMaybe
  , clearBit
  , complement
  , complementBit
  , countLeadingZeros
  , countTrailingZeros
  , finiteBitSize
  , isSigned
  , popCount
  , popCountDefault
  , rotate
  , rotateL
  , rotateR
  , setBit
  , shift
  , shiftL
  , shiftR
  , testBit
  , testBitDefault
  , toIntegralSized
  , xor
  , zeroBits
  , (.&.)
  , (.|.)
  )
import Data.Bool as Bool
  ( Bool (False, True)
  , not
  , otherwise
  , (&&)
  , (||)
  )
import Data.ByteString as ByteString (ByteString)
import Data.ByteString.Lazy qualified
import Data.Char as Char
  ( Char
  , chr
  , digitToInt
  , intToDigit
  , isAlpha
  , isAlphaNum
  , isAscii
  , isControl
  , isDigit
  , isHexDigit
  , isLetter
  , isLower
  , isPrint
  , isSpace
  , isUpper
  , ord
  , toLower
  , toTitle
  , toUpper
  )
import Data.Complex as Complex
  ( Complex ((:+))
  , cis
  , conjugate
  , imagPart
  , magnitude
  , mkPolar
  , phase
  , polar
  , realPart
  )
import Data.Either as Either
  ( Either (Left, Right)
  , either
  , isLeft
  , isRight
  , lefts
  , partitionEithers
  , rights
  )
import Data.Function as Function
  ( const
  , fix
  , flip
  , on
  , ($)
  , (&)
  , (.)
  )
import Data.Hashable as Hashable
  ( Hashable
  , hash
  , hashUsing
  , hashWithSalt
  )
import Data.Int as Int
  ( Int
  , Int16
  , Int32
  , Int64
  , Int8
  )
import Data.IntMap as DataStructures (IntMap)
import Data.IntSet as DataStructures (IntSet)
import Data.List as List
  ( break
  , cycle
  , drop
  , dropWhile
  , filter
  , genericDrop
  , genericLength
  , genericReplicate
  , genericSplitAt
  , genericTake
  , group
  , inits
  , intercalate
  , intersperse
  , isInfixOf
  , isPrefixOf
  , isSuffixOf
  , iterate
  , permutations
  , repeat
  , replicate
  , reverse
  , scanl
  , scanl'
  , scanr
  , sort
  , sortBy
  , splitAt
  , subsequences
  , tails
  , take
  , takeWhile
  , transpose
  , unfoldr
  , unzip
  , zip
  , zipWith
  )
import Data.Map as DataStructures (Map)
import Data.Maybe as Maybe
  ( Maybe (Just, Nothing)
  , catMaybes
  , fromMaybe
  , isJust
  , isNothing
  , listToMaybe
  , mapMaybe
  , maybe
  , maybeToList
  )
import Data.Monoid as Monoid
import Data.Proxy as Typelevel
  ( Proxy (..)
  )
import Data.Sequence as DataStructures (Seq)
import Data.Set as DataStructures (Set)
import Data.Text as Text
  ( Text
  , lines
  , unlines
  , unwords
  , words
  )
import Data.Text.Encoding as Text
  ( decodeUtf8
  , decodeUtf8'
  , decodeUtf8With
  , encodeUtf8
  )
import Data.Text.Encoding.Error as Text
  ( OnDecodeError
  , OnError
  , UnicodeException
  , ignore
  , lenientDecode
  , replace
  , strictDecode
  )
import Data.Text.IO as Text
  ( appendFile
  , getContents
  , getLine
  , interact
  , readFile
  , writeFile
  )
import Data.Text.Lazy as Text
  ( fromStrict
  , toStrict
  )
import Data.Text.Lazy qualified
import Data.Tuple as Tuple
  ( curry
  , fst
  , snd
  , swap
  , uncurry
  )
import Data.Type.Coercion as Typelevel
  ( Coercion (..)
  , coerceWith
  , repr
  )
import Data.Type.Equality as Typelevel
  ( castWith
  , gcastWith
  , sym
  , trans
  , (:~:) (..)
  , type (==)
  )
import Data.Void as Typelevel
  ( Void
  , absurd
  , vacuous
  )
import Data.Word as Bits
  ( Word
  , Word16
  , Word32
  , Word64
  , Word8
  , byteSwap16
  , byteSwap32
  , byteSwap64
  )
import Foreign.Ptr as Foreign (IntPtr, WordPtr)
import Foreign.StablePtr as Foreign (StablePtr)
import Foreign.Storable as Foreign (Storable)
import GHC.Generics as Generics
  ( Associativity (..)
  , C1
  , Constructor (..)
  , D1
  , Datatype (..)
  , Fixity (..)
  , FixityI (..)
  , Generic (..)
  , Generic1
  , Generically (..)
  , K1 (..)
  , M1 (..)
  , Meta (..)
  , Rec0
  , Rep
  , S1
  , Selector (..)
  , U1 (..)
  , URec
  , V1
  , (:*:) (..)
  , (:+:) (..)
  , (:.:) (..)
  )
import System.Environment as System (getArgs)
import System.Exit as System
  ( ExitCode (..)
  , exitFailure
  , exitSuccess
  , exitWith
  )
import System.Exit qualified
import System.IO as System
  ( FilePath
  , Handle
  , IOMode (..)
  , openFile
  , stderr
  , stdin
  , stdout
  , withFile
  )
import Text.Read as Read
  ( Read
  , reads
  )
import Text.Read qualified as Read
import Type.Reflection as Typeable
  ( TypeRep
  , Typeable
  , typeOf
  , typeRep
  )
import UnliftIO.Async as Concurrency
  ( Async (..)
  , AsyncCancelled (..)
  , Concurrently (..)
  , async
  , asyncBound
  , asyncOn
  , asyncThreadId
  , cancel
  , cancelWith
  , concurrently
  , link
  , link2
  , poll
  , race
  , race_
  , wait
  , waitAny
  , waitAnyCancel
  , waitAnyCatch
  , waitAnyCatchCancel
  , waitBoth
  , waitCatch
  , waitEither
  , waitEitherCancel
  , waitEitherCatch
  , waitEitherCatchCancel
  , waitEither_
  , withAsync
  , withAsyncBound
  , withAsyncOn
  )
import UnliftIO.Chan as Concurrency
  ( Chan
  , dupChan
  , getChanContents
  , newChan
  , readChan
  , writeChan
  , writeList2Chan
  )
import UnliftIO.Concurrent as Concurrency
  ( ThreadId
  , forkFinally
  , forkIO
  , forkIOWithUnmask
  , forkOS
  , forkOn
  , forkOnWithUnmask
  , getNumCapabilities
  , isCurrentThreadBound
  , killThread
  , mkWeakThreadId
  , myThreadId
  , rtsSupportsBoundThreads
  , runInBoundThread
  , runInUnboundThread
  , setNumCapabilities
  , threadCapability
  , threadDelay
  , threadWaitRead
  , threadWaitWrite
  , yield
  )
import UnliftIO.Exception as Exception
  ( Exception
  , IOException
  , SomeAsyncException (SomeAsyncException)
  , SomeException (SomeException)
  , asyncExceptionFromException
  , asyncExceptionToException
  , bracket
  , bracketOnError
  , bracket_
  , catch
  , catchJust
  , catches
  , displayException
  , evaluate
  , finally
  , fromException
  , handle
  , handleJust
  , mask
  , mask_
  , onException
  , toException
  , try
  , tryJust
  , uninterruptibleMask
  , uninterruptibleMask_
  )
import UnliftIO.IORef as Concurrency
import UnliftIO.MVar as Concurrency
  ( MVar
  , isEmptyMVar
  , mkWeakMVar
  , modifyMVar
  , modifyMVarMasked
  , modifyMVarMasked_
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , swapMVar
  , takeMVar
  , tryPutMVar
  , tryReadMVar
  , tryTakeMVar
  , withMVar
  , withMVarMasked
  )
import UnliftIO.QSem as Concurrency
  ( QSem
  , newQSem
  , signalQSem
  , waitQSem
  )
import UnliftIO.QSemN as Concurrency
  ( QSemN
  , newQSemN
  , signalQSemN
  , waitQSemN
  )

import Unsafe.Coerce

-- Type synonymss for lazy texts
type LText = Data.Text.Lazy.Text

type LByteString = Data.ByteString.Lazy.ByteString

-- | The identity function, returns the give value unchanged.
identity :: a -> a
identity x = x

map :: (Functor.Functor f) => (a -> b) -> f a -> f b
map = Functor.fmap

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

unsnoc :: [x] -> Maybe ([x], x)
unsnoc = Foldable.foldr go Nothing
  where
    go x mxs =
      Just
        ( case mxs of
            Nothing -> ([], x)
            Just (xs, e) -> (x : xs, e)
        )

-- | Apply a function n times to a given value
applyN :: Int -> (a -> a) -> a -> a
applyN n f = Foldable.foldr (.) identity (List.replicate n f)

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
--
-- >>> readMaybe ("123" :: Text) :: Maybe Int
-- Just 123
--
-- >>> readMaybe ("hello" :: Text) :: Maybe Int
-- Nothing
readMaybe :: (Read b, Conv.StringConv a String) => a -> Maybe b
readMaybe = Read.readMaybe . Conv.toS

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
-- A 'Left' value indicates a parse error.
--
-- >>> readEither "123" :: Either Text Int
-- Right 123
--
-- >>> readEither "hello" :: Either Text Int
-- Left "Prelude.read: no parse"
readEither :: (Read a, Conv.StringConv String e, Conv.StringConv e String) => e -> Either e a
readEither = first Conv.toS . Read.readEither . Conv.toS

-- | The print function outputs a value of any printable type to the standard
-- output device. Printable types are those that are instances of class Show;
-- print converts values to strings for output using the show operation and adds
-- a newline.
print :: (Trans.MonadIO m, PBase.Show a) => a -> m ()
print = liftIO . PBase.print

-- | Do nothing returning unit inside applicative.
pass :: (Applicative f) => f ()
pass = pure ()

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded p x = Bool.bool empty (pure x) (p x)

guardedA :: (Functor.Functor f, Alternative t) => (a -> f Bool) -> a -> f (t a)
guardedA p x = Bool.bool empty (pure x) `Functor.fmap` p x

-- | Lift an 'IO' operation with 1 argument into another monad
liftIO1 :: (MonadIO m) => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO

-- | Lift an 'IO' operation with 2 arguments into another monad
liftIO2 :: (MonadIO m) => (a -> b -> IO c) -> a -> b -> m c
liftIO2 = ((.) . (.)) liftIO

show :: (Show a, Conv.StringConv String b) => a -> b
show x = Conv.toS (PBase.show x)
{-# SPECIALIZE show :: (Show a) => a -> Text #-}
{-# SPECIALIZE show :: (Show a) => a -> LText #-}
{-# SPECIALIZE show :: (Show a) => a -> String #-}

#if MIN_VERSION_base(4,8,0)
-- | Terminate main process with failure
die :: Text -> IO a
die err = System.Exit.die (ConvertText.toS err)
#else
-- | Terminate main process with failure
die :: Text -> IO a
die err = hPutStrLn stderr err >> exitFailure
#endif

#if !MIN_VERSION_base(4,8,0)
-- This is a literal copy of the implementation in GHC.List in base-4.10.1.0.

-- | A strictly accumulating version of 'scanl'
{-# NOINLINE [1] scanl' #-}
scanl'           :: (b -> a -> b) -> b -> [a] -> [b]
scanl' = scanlGo'
  where
    scanlGo'           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo' f !q ls    = q : (case ls of
                            []   -> []
                            x:xs -> scanlGo' f (f q x) xs)

{-# RULES
"scanl'"  [~1] forall f a bs . scanl' f a bs =
  build (\c n -> a `c` foldr (scanlFB' f c) (flipSeqScanl' n) bs a)
"scanlList'" [1] forall f a bs .
    foldr (scanlFB' f (:)) (flipSeqScanl' []) bs a = tail (scanl' f a bs)
 #-}

{-# INLINE [0] scanlFB' #-}
scanlFB' :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB' f c = \b g -> \x -> let !b' = f x b in b' `c` g b'

{-# INLINE [0] flipSeqScanl' #-}
flipSeqScanl' :: a -> b -> a
flipSeqScanl' a !_b = a
#endif

atomicModifyIORef'_ :: (MonadIO m) => IORef a -> (a -> a) -> m ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref ((,()) . f)

mapUnliftIO :: (forall a. m a -> n a) -> UnliftIO n -> UnliftIO m
mapUnliftIO nt (UnliftIO f) = UnliftIO $ f . nt

loopM :: (Monad m) => (a -> m (Either a b)) -> a -> m b
loopM act x = do
  res <- act x
  case res of
    Left x' -> loopM act x'
    Right v -> pure v
