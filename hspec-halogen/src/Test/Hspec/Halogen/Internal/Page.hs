{-# LANGUAGE CPP #-}

-- | Everything "Test.Hspec.Halogen" asks of the page it runs in.
--
-- The WebAssembly and JavaScript backends have a page to ask, when the suite
-- runs in Chromium: through "Test.Hspec.Halogen.Internal.Wasm" and
-- "Test.Hspec.Halogen.Internal.JS". Natively the package still builds, so that
-- the suites using it type-check and the Haskell language server can load
-- them, but nothing here can run: 'inBrowser' is 'False' and the rest panics.
module Test.Hspec.Halogen.Internal.Page
  ( inBrowser
  , runnerArgs
  , reportDone
  , removeLeftovers
  , createContainer
  , createContainerIn
  , removeElement
  , querySelector
  , querySelectorAll
  , act
  , pressKey
  , focusElement
  , blurElement
  , settleTasks
  , textContentOf
  , propertyOf
  , attributeOf
  , outerHTMLOf
  , checkVisibility
  , sameElement
  , isConnected
  )
where

#if defined(wasm32_HOST_ARCH)

import Test.Hspec.Halogen.Internal.Wasm

#elif defined(javascript_HOST_ARCH)

import Test.Hspec.Halogen.Internal.JS

#else

import Protolude
import Web.DOM.Internal.Types (Element)

-- | Whether there is a page to run in.
inBrowser :: IO Bool

-- | hspec's arguments, when the test runner passed some. Browser GHCi passes
-- none: there @:main@ sets them.
runnerArgs :: IO (Maybe [Text])

-- | Tell the runner how many tests failed.
reportDone :: Int -> IO ()

-- | Remove the containers of a run that was interrupted before cleaning up.
removeLeftovers :: IO ()

-- | A fresh container at the end of the body.
createContainer :: IO Element

-- | A fresh container at the end of another.
createContainerIn :: Element -> IO Element

removeElement :: Element -> IO ()

querySelector :: Element -> Text -> IO (Maybe Element)

querySelectorAll :: Element -> Text -> IO [Element]

-- | Have the runner click, type into or clear an element, and wait until it
-- has.
act :: Text -> Element -> Text -> IO ()

-- | Have the runner press a key on the focused element, and wait until it has.
pressKey :: Text -> IO ()

focusElement :: Element -> IO ()

blurElement :: Element -> IO ()

-- | Wait until every task the page has queued at normal priority has run.
settleTasks :: IO ()

textContentOf :: Element -> IO Text

-- | A property rendered with JavaScript's @String()@.
propertyOf :: Element -> Text -> IO Text

attributeOf :: Element -> Text -> IO (Maybe Text)

outerHTMLOf :: Element -> IO Text

checkVisibility :: Element -> IO Bool

-- | Whether two handles are the same node (JavaScript's @===@).
sameElement :: Element -> Element -> Bool

-- | Whether the element is still in the document.
isConnected :: Element -> IO Bool

inBrowser = pure False

runnerArgs = pure Nothing

reportDone _ = pass

removeLeftovers = pass

createContainer = needsBrowser

createContainerIn _ = needsBrowser

removeElement _ = needsBrowser

querySelector _ _ = needsBrowser

querySelectorAll _ _ = needsBrowser

act _ _ _ = needsBrowser

pressKey _ = needsBrowser

focusElement _ = needsBrowser

blurElement _ = needsBrowser

settleTasks = needsBrowser

textContentOf _ = needsBrowser

propertyOf _ _ = needsBrowser

attributeOf _ _ = needsBrowser

outerHTMLOf _ = needsBrowser

checkVisibility _ = needsBrowser

sameElement _ _ = needsBrowser

isConnected _ = needsBrowser

needsBrowser :: (HasCallStack) => a
needsBrowser = panic "hspec-halogen needs a browser: run the suite on the WebAssembly backend (npm run test-wasm)"

#endif
