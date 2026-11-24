-- | The Chrome DevTools Protocol, and a processor slowed down with it.
module Test.DevTools (spec) where

import Control.Monad (replicateM)
import Data.Aeson (FromJSON (..), Value, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import Halogen.Playwright
import Halogen.Playwright.Hspec

-- | What @Runtime.evaluate@ says of an element.
newtype Remote = Remote Text

instance FromJSON Remote where
  parseJSON = withObject "result" $ \o -> o .: "result" >>= withObject "object" (\r -> Remote <$> r .: "objectId")

newtype Listeners = Listeners [Text]

instance FromJSON Listeners where
  parseJSON = withObject "listeners" $ \o -> Listeners <$> (o .: "listeners" >>= mapM (withObject "listener" (.: "type")))

newtype Authenticator = Authenticator Text

instance FromJSON Authenticator where
  parseJSON = withObject "authenticator" $ \o -> Authenticator <$> o .: "authenticatorId"

-- | How many rounds of a small fixed computation the page gets through in
-- 200 ms of its own clock. A processor throttle stops the page's thread for
-- part of every stretch of time, so fewer rounds fit in it. The call takes
-- 200 ms however fast the machine is, so it cannot outlast the timeout; a
-- fixed computation timed instead took over 2 s under a sixfold throttle on
-- a machine under load.
rounds :: Page -> IO Int
rounds page =
  evaluate
    page
    "(() => { const end = performance.now() + 200; let n = 0, x = 0; while (performance.now() < end) { for (let i = 0; i < 1000; i++) x = (x + i * 7) % 1000003; n++; } window.sink = x; return n; })()"

-- | Whether a page got through fewer than 1/2.5 of the rounds it gets
-- through at full speed, from measurements taken on the same page: the
-- best of each kind, so that a moment of load on the machine during one
-- measurement does not decide the outcome. Both kinds are slowed alike by
-- a slow machine; only the throttle tells them apart.
slowedDown :: [Int] -> [Int] -> Bool
slowedDown full slowed = fromIntegral (maximum full) > 2.5 * (fromIntegral (maximum slowed) :: Double)

spec :: Text -> SpecWith Playwright
spec base = describe "the DevTools protocol" $ do
  let at path = base <> path

  -- smoke-test.mjs and smoke-test-server.mjs: whether the program has
  -- adopted the page, read as the click listener it attaches, which only
  -- the protocol sees.
  it "calls a method of the page's session and decodes its result" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    Remote objectId <- cdp page "Runtime.evaluate" (object ["expression" .= ("document.getElementById('count')" :: Text)])
    Listeners kinds <- cdp page "DOMDebugger.getEventListeners" (object ["objectId" .= objectId])
    kinds `shouldBe` ["click"]
    _ :: Value <- cdp page "Runtime.releaseObject" (object ["objectId" .= objectId])
    -- An unknown method is the protocol's error.
    (cdp page "Nothing.atAll" () :: IO Value) `shouldThrow` \(e :: PlaywrightError) -> e.method == "cdp"

  -- The wiki's smoke test: what crossed the page's network, heard as the
  -- protocol's events.
  it "keeps the events listened to, and waits for one" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    listenCDP page "Network.requestWillBeSent"
    -- Twice is once.
    listenCDP page "Network.requestWillBeSent"
    _ :: Value <- cdp page "Network.enable" ()
    _ :: Value <- evaluate page "fetch('second.html?listened').then(() => null)"
    let toSecond v = Just (at "second.html?listened") == parseMaybe (withObject "event" (\o -> o .: "request" >>= (.: "url"))) v
    sent <- waitForCDP page "Network.requestWillBeSent" toSecond
    parseMaybe (withObject "event" (\o -> o .: "request" >>= (.: "method"))) sent `shouldBe` Just ("GET" :: Text)
    filter toSecond <$> cdpEvents page "Network.requestWillBeSent" >>= (`shouldSatisfy` ((== 1) . length))
    -- Another event, not listened to, is not kept; a wait for it says so.
    cdpEvents page "Network.responseReceived" `shouldReturn` []
    (waitForCDP (withTimeout 300 page) "Network.responseReceived" (const True) >> pure ())
      `shouldThrow` \(e :: PlaywrightError) -> e.name == "TimeoutError"

  -- test-passkeys.mjs: a virtual authenticator, through which a real
  -- passkey is made and used.
  it "keeps one session per page, for a virtual authenticator" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    _ :: Value <- cdp page "WebAuthn.enable" ()
    let options = object ["protocol" .= ("ctap2" :: Text), "transport" .= ("internal" :: Text), "hasResidentKey" .= True, "hasUserVerification" .= True, "isUserVerified" .= True]
    Authenticator first' <- cdp page "WebAuthn.addVirtualAuthenticator" (object ["options" .= options])
    _ :: Value <- cdp page "WebAuthn.removeVirtualAuthenticator" (object ["authenticatorId" .= first'])
    Authenticator second' <- cdp page "WebAuthn.addVirtualAuthenticator" (object ["options" .= options])
    second' `shouldNotBe` first'

  -- The wiki's plan (WP5.98): the smoke tests run with the processor
  -- throttled, so that races are found on purpose.
  it "slows a page's processor down" $ \pw -> withPage pw $ \page -> do
    goto page (at "basics.html")
    -- Full speed and throttled in turn, on the same page; full speed first,
    -- whatever throttle the environment puts on every page.
    (full, slowed) <- unzip <$> replicateM 3 ((,) <$> (throttleCPU page 1 >> rounds page) <*> (throttleCPU page 6 >> rounds page))
    throttleCPU page 1
    (full, slowed) `shouldSatisfy` uncurry slowedDown

  -- The page starts throttled by the options; taken back to full speed by
  -- 'throttleCPU', it gets through more.
  it "slows every page down when the options say so" $ \_ -> do
    options <- optionsFromEnvironment
    withPlaywright options {throttle = Just 6} $ \pw -> withPage pw $ \page -> do
      goto page (at "basics.html")
      slowed <- replicateM 3 (rounds page)
      throttleCPU page 1
      full <- replicateM 3 (rounds page)
      (full, slowed) `shouldSatisfy` uncurry slowedDown
