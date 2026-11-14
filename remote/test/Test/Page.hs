-- | What the session keeps of the page itself: where it is, which the
-- page says first and whenever it goes back or forward, and what its
-- stores hold of this library's, which the program reads from that mirror
-- and writes through operations the page performs.
--
-- The stores are the session's on every backend. The address is read
-- through the window's native objects, so natively only: in a browser
-- (under Node, on both browser backends) a router reads the engine's own
-- address, as in the loopback.
module Test.Page (spec) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Halogen.JSBits (inBrowser)
import Halogen.Remote.Internal (runRemoteDOM)
import Halogen.Remote.Internal qualified as Internal
import Halogen.Remote.Protocol (EventMessage (..), FromPage (..), Hello (..), HistoryMove (..), Op (..), encodeFromPage)
import Halogen.Remote.Session qualified as Session
import Halogen.Router qualified as Router
import Protolude
import Test.Components (Spot (..), routing, spots)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, xdescribe)
import Test.Session (newWire, nextOps, required)
import Web.Storage.Storage qualified as Storage

-- | The page's first message: at a post, under a base, with a count kept in
-- local storage (4, whose JSON is @4@, in base64) and something of another
-- program's, which a client does not send but a test may.
hello :: Text
hello = encodeFromPage (PageHello greeting)

greeting :: Hello
greeting =
  Hello
    { location = "http://example.test/site/blog/post/3?x=1#top"
    , base = Just "http://example.test/site/"
    , local = Map.fromList [("haskell-halogen:count", "NA=="), ("someone-else", "theirs")]
    , session = Map.fromList [("haskell-halogen:visits", "Mg==")]
    }

count :: Storage.StorageKind -> Text -> Session.Session -> IO (Maybe Int)
count kind key session = (>>= rightToMaybe) <$> Session.run session (Storage.getItem kind key)

spec :: Spec
spec = describe "the page, as the session keeps it" $ do
  describe "its stores" $ do
    it "are empty without the page's first message" $ do
      (_, send) <- newWire
      session <- Session.newSession send
      count Storage.LocalStorage "count" session `shouldReturn` Nothing
      Session.run session (Storage.keys Storage.LocalStorage) `shouldReturn` []
      Session.dispose session

    it "are what the page's first message says, before anything is mounted" $ do
      (_, send) <- newWire
      session <- Session.newSession send
      Session.receive session hello
      count Storage.LocalStorage "count" session `shouldReturn` Just 4
      count Storage.SessionStorage "visits" session `shouldReturn` Just 2
      Session.run session (Storage.keys Storage.LocalStorage) `shouldReturn` ["count"]
      Session.dispose session

    it "send what the program writes and removes, for the page to do, and read it back at once" $ do
      recorder <- Internal.newRecorder
      Internal.greet recorder greeting
      runRemoteDOM recorder $ do
        Storage.setItem Storage.LocalStorage "count" (5 :: Int)
        Storage.removeItem Storage.SessionStorage "visits"
      runRemoteDOM recorder (Storage.getItem Storage.LocalStorage "count") `shouldReturn` Just (Right (5 :: Int))
      runRemoteDOM recorder (Storage.member Storage.SessionStorage "visits") `shouldReturn` False
      ops <- fold <$> Internal.takeBatch recorder
      ops
        `shouldBe` [ StorageOp Storage.LocalStorage "haskell-halogen:count" (Just "NQ==")
                   , StorageOp Storage.SessionStorage "haskell-halogen:visits" Nothing
                   ]

    it "follow another tab's changes, and a store emptied" $ do
      (_, send) <- newWire
      session <- Session.newSession send
      Session.receive session hello
      Session.receive session (encodeFromPage (PageStorage Storage.LocalStorage (Just "haskell-halogen:count") (Just "Nw==")))
      count Storage.LocalStorage "count" session `shouldReturn` Just 7
      Session.receive session (encodeFromPage (PageStorage Storage.LocalStorage Nothing Nothing))
      count Storage.LocalStorage "count" session `shouldReturn` Nothing
      count Storage.SessionStorage "visits" session `shouldReturn` Just 2
      Session.dispose session

  (if inBrowser then xdescribe else describe) "its address" $ do
    it "is the root, with no base, without the page's first message" $ do
      (_, send) <- newWire
      session <- Session.newSession send
      Session.run session Router.pageBase `shouldReturn` "/"
      router <- Session.run session (Router.newRouter "/" Front spots)
      Router.current router `shouldReturn` Front
      Router.stop router
      Session.dispose session

    it "is where the page's first message says, under the base it names" $ do
      (_, send) <- newWire
      session <- Session.newSession send
      Session.receive session hello
      Session.run session Router.pageBase `shouldReturn` "/site/"
      router <- Session.run session (Router.newRouter "/site/" Front spots)
      Router.current router `shouldReturn` Post 3
      Router.stop router
      Session.dispose session

    it "moves at once when the program goes somewhere, and the page is told to push or replace" $ do
      recorder <- Internal.newRecorder
      Internal.greet recorder greeting
      (router, hashed) <- runRemoteDOM recorder ((,) <$> Router.newRouter "/site/" Front spots <*> Router.newHashRouter Front spots)
      _ <- Internal.takeBatch recorder
      Router.navigate router About
      Router.current router `shouldReturn` About
      Internal.pageAddress recorder `shouldReturn` "http://example.test/site/about"
      Router.redirect router Front
      Internal.pageAddress recorder `shouldReturn` "http://example.test/site/"
      -- A hash router writes the page's path, and its fragment.
      Router.navigate hashed (Post 2)
      Internal.pageAddress recorder `shouldReturn` "http://example.test/site/#/blog/post/2"
      Router.current hashed `shouldReturn` Post 2
      ops <- fold <$> Internal.takeBatch recorder
      ops
        `shouldBe` [ HistoryOp (Push (Just "/site/about"))
                   , HistoryOp (Replace (Just "/site/"))
                   , HistoryOp (Push (Just "/site/#/blog/post/2"))
                   ]
      Router.stop router
      Router.stop hashed

    it "is where the page says it went before its popstate reaches a component" $ do
      (wire, send) <- newWire
      session <- Session.newSession send
      Session.receive session hello
      router <- Session.run session (Router.newRouter "/site/" Front spots)
      _ <- Session.mount session routing router
      ops <- nextOps wire
      shown <- required "the spot's text" (head [n | CreateText n "Post 3" <- ops])
      popstate <- required "the router's popstate listener" (head [l | Listen 0 "popstate" l <- ops])
      Session.receive session (encodeFromPage (PageLocation "http://example.test/site/about"))
      Session.receive session (encodeFromPage (PageEvent EventMessage {listener = popstate, node = 0, eventType = "popstate", event = Aeson.object ["type" Aeson..= ("popstate" :: Text)]}))
      nextOps wire `shouldReturn` [SetText shown "About"]
      Router.current router `shouldReturn` About
      Router.stop router
      Session.dispose session
