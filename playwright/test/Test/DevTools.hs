-- | The Chrome DevTools Protocol, and a processor slowed down with it.
module Test.DevTools (spec) where

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

-- | Milliseconds the page takes over a fixed computation, measured by the
-- page's own clock.
computation :: Page -> IO Double
computation page =
  evaluate
    page
    "(() => { const t = performance.now(); let x = 0; for (let i = 0; i < 3e7; i++) x = (x + i * 7) % 1000003; window.sink = x; return performance.now() - t; })()"

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
    _ <- computation page
    full <- computation page
    throttleCPU page 6
    slowed <- computation page
    throttleCPU page 1
    slowed `shouldSatisfy` (> full * 2.5)

  it "slows every page down when the options say so" $ \_ -> do
    options <- optionsFromEnvironment
    let measure o = withPlaywright o $ \pw -> withPage pw $ \page -> do
          goto page (at "basics.html")
          _ <- computation page
          computation page
    full <- measure options {throttle = Nothing}
    slowed <- measure options {throttle = Just 6}
    slowed `shouldSatisfy` (> full * 2.5)
