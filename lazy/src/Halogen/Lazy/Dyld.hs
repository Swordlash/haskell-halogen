{-# LANGUAGE TemplateHaskell #-}

-- | Code the page loads on demand: on WebAssembly, a shared library that
-- GHC's dynamic linker (@dyld.mjs@) links into the running program, and
-- that hands over a value, a component say, for "Halogen.Lazy" to show.
--
-- > feature :: Dyld.Library
-- > feature = Dyld.library "lib/libfeature.so" "feature_entry"
-- >
-- > HH.slot "feature" () (Lazy.lazy Lazy.defaultLoading (liftIO (Dyld.load feature))) input Heard
--
-- = The library's side
--
-- The library is a module compiled with @-shared -dynamic@ into a @.so@,
-- against the very libraries the program was built with (it links to them,
-- by their file names, rather than carrying copies), and it exports one
-- function, its entry, that gives what it offers as a 'Dynamic':
--
-- > foreign export javascript "feature_entry" entry :: IO (StablePtr Dynamic)
-- >
-- > entry :: IO (StablePtr Dynamic)
-- > entry = Dyld.offer (component :: H.Component Query Input Output BrowserDOM)
--
-- 'load' checks that the 'Dynamic' holds a value of the type it was asked
-- for, so the program and the library must agree on that type, every type
-- in it included: the query, input and output types belong in a library
-- both are built against (the program's own, or one they share), not in
-- the shared library itself, whose types the program cannot name. Compile
-- the library with a unit id of its own (@-this-unit-id@), so that its
-- modules' symbols cannot clash with another library's or the program's.
--
-- dyld keeps the exports of every library it has linked in one table, so an
-- entry's name must be unique among them, and it knows a library by its
-- file name, which must be unique too.
--
-- = Where it works
--
-- Only in a program that GHC's dyld started, in a page that says so (see
-- 'available'): the tutorial's page, which runs GHC itself as a shared
-- library, is one. A program linked into one @.wasm@ (every example here,
-- and what a release build makes) has no linker to link more into it, and
-- neither has a program of the JavaScript backend nor a native one; there
-- 'load' throws 'NotAvailable', and "Halogen.Lazy" works as well with any
-- other loader.
module Halogen.Lazy.Dyld
  ( -- * Loading a library
    Library (..)
  , library
  , load
  , available
  , LazyError (..)

    -- * The library's side
  , offer
  )
where

import Data.Dynamic (Dynamic, dynTypeRep, fromDynamic, toDyn)
import Data.Text qualified as Text
import Foreign.Ptr (intPtrToPtr)
import Foreign.StablePtr (castPtrToStablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Halogen.JSBits (JSText, JSVal, Safety (..), browserJS, toJSText)
import Halogen.JSBits.Promise (JSError (..), Promise (..), await)
import Protolude

$( browserJS
     ["jsbits/lazy.js"]
     [ ("js_available", "halogen_lazy_available", Unsafe, [t|IO Bool|])
     , ("js_load", "halogen_lazy_load", Unsafe, [t|JSText -> JSText -> JSText -> IO Promise|])
     , ("js_number", "halogen_lazy_number", Unsafe, [t|JSVal -> IO Int|])
     , ("js_abandon", "halogen_lazy_abandon", Unsafe, [t|Promise -> IO ()|])
     ]
 )

-- | A shared library to load, and what to call in it.
data Library = Library
  { url :: Text
  -- ^ Where the page fetches it from, relative to the page's base address
  -- or absolute, as for @fetch@.
  , path :: Text
  -- ^ The absolute path in the program's file system where it is put for
  -- dyld, which reads libraries from there. Its file name is the library's
  -- name to dyld, which links a library of a name only once.
  , entry :: Text
  -- ^ The name of the function it exports that hands its value over (see
  -- 'offer').
  }
  deriving stock (Eq, Show)

-- | A library at this address, with this entry, put at
-- @\/tmp\/halogen-lazy\/@ and its file name in the program's file system.
library :: Text -> Text -> Library
library url entry = Library {url, path = "/tmp/halogen-lazy/" <> fileName, entry}
  where
    fileName = Text.takeWhileEnd (/= '/') (Text.takeWhile (\c -> c /= '?' && c /= '#') url)

-- | Why 'load' failed.
data LazyError
  = -- | The program was not started by GHC's dyld, or the page did not hand
    -- it over (see 'available'): natively, on the JavaScript backend, and in
    -- a program linked into one @.wasm@.
    NotAvailable
  | -- | Fetching the library, linking it or calling its entry failed: what
    -- the page said, such as a missing file, a symbol no library defines,
    -- or an entry the library does not export.
    LinkFailed Text
  | -- | The entry handed over a value of another type than the one asked
    -- for.
    WrongType {expected :: TypeRep, actual :: TypeRep}
  deriving stock (Eq, Show)

instance Exception LazyError where
  displayException = \case
    NotAvailable -> "Halogen.Lazy.Dyld: this program has no dynamic linker to load a library with"
    LinkFailed message -> "Halogen.Lazy.Dyld: " <> Text.unpack message
    WrongType {expected, actual} -> "Halogen.Lazy.Dyld: the library gave a " <> show actual <> ", not a " <> show expected

-- | Whether this program can 'load' a library: it was started by GHC's dyld,
-- and the page that started it handed dyld over by setting
-- @globalThis.halogenDyld = { dyld, rootfs, shim }@ (dyld as @dyld.mjs@'s
-- @main@ gave it, the @browser_wasi_shim@ directory dyld's host reads files
-- from, and that @browser_wasi_shim@ module). False natively and on the
-- JavaScript backend.
available :: IO Bool
available = js_available

-- | Fetch the library, put it in the file system, have dyld link it into the
-- program (with what it needs that is not linked yet) and call its entry,
-- and give what the entry handed over, if it is a @c@. A library is fetched
-- and linked once per path; a later 'load' of it only calls its entry
-- again. It waits for the fetch and the link, so call it where a program
-- may wait (in @lift@ or @liftIO@, which is where "Halogen.Lazy" runs its
-- loader).
--
-- A fetch that failed is tried again by the next 'load'. A link that failed
-- is not, for as long as the page lives: dyld marks a library's name as
-- linked before it links it, and has no way to forget it, so linking it
-- again would do nothing. Every later 'load' of the path fails with the
-- first failure's 'LinkFailed', and only reloading the page tries again.
--
-- Throws a 'LazyError': 'NotAvailable' where there is no dyld, 'LinkFailed'
-- when the page could not fetch, link or call it, and 'WrongType' when the
-- entry gave something else than a @c@.
load :: forall c. (Typeable c) => Library -> IO c
load lib = do
  ok <- available
  if not ok
    then throwIO NotAvailable
    else mask $ \restore -> do
      promise <- js_load (toJSText lib.url) (toJSText lib.path) (toJSText lib.entry)
      -- The thread may be cancelled while it waits (the wrapper taken off
      -- the page before the library arrived): the fetch and the link go on,
      -- the entry is still called, and the StablePtr it makes would have
      -- nobody to free it, so the page is told to free it when it comes.
      -- From here to the free below, nothing else can interrupt.
      outcome <- restore (await promise) `onException` js_abandon promise
      case outcome of
        Left e
          | e.name == "NotAvailableError" -> throwIO NotAvailable
          | otherwise -> throwIO (LinkFailed (e.name <> ": " <> e.message))
        Right value -> do
          pointer <- js_number value
          let stable = castPtrToStablePtr (intPtrToPtr (fromIntegral pointer)) :: StablePtr Dynamic
          dynamic <- deRefStablePtr stable
          freeStablePtr stable
          case fromDynamic dynamic of
            Just c -> pure c
            Nothing -> throwIO WrongType {expected = typeRep (Proxy :: Proxy c), actual = dynTypeRep dynamic}

-- | What a library's entry returns: the value it offers, in a 'Dynamic' for
-- 'load' to check the type of, behind a 'StablePtr', which 'load' frees.
-- Each call makes a new one, so the entry can be called again (by a second
-- 'load', in a program run again).
offer :: forall c. (Typeable c) => c -> IO (StablePtr Dynamic)
offer = newStablePtr . toDyn
