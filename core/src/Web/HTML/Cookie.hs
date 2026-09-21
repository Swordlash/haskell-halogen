-- | Cookies, one at a time.
--
-- The DOM offers @document.cookie@, a string that reads as every cookie at
-- once and writes as one cookie with its attributes. This is that, in terms of
-- cookies: 'Web.HTML.HTMLDocument.cookie' and
-- 'Web.HTML.HTMLDocument.setCookie' remain underneath for anything unusual.
--
-- Names and values are percent-encoded on the way out and decoded on the way
-- in, because a cookie string has no other way to carry a @;@ or a @,@ — and
-- because a browser that finds one will simply truncate the cookie.
module Web.HTML.Cookie
  ( -- * Reading
    getCookies
  , getCookie

    -- * Writing
  , Cookie (..)
  , SameSite (..)
  , defaultCookie
  , setCookie
  , deleteCookie

    -- * The strings themselves
  , parseCookies
  , renderCookie
  , encodeComponent
  , decodeComponent
  )
where

import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HPrelude
import Web.DOM.Internal.Types (HTMLDocument)
import Web.HTML.HTMLDocument qualified as HTMLDocument

-- | When a cookie is sent with a request the browser did not start itself.
data SameSite
  = Strict
  | Lax
  | None
  deriving stock (Eq, Show)

-- | A cookie to write. 'defaultCookie' is the one with no attributes at all,
-- which the browser keeps for the session and sends with every request to the
-- page's own origin.
data Cookie = Cookie
  { name :: Text
  , value :: Text
  , maxAge :: Maybe Int
  -- ^ How long to keep it, in seconds. Nothing keeps it for the session.
  , path :: Maybe Text
  , domain :: Maybe Text
  , sameSite :: Maybe SameSite
  , secure :: Bool
  -- ^ Send it over HTTPS only.
  }
  deriving stock (Eq, Show)

-- | A cookie with a name and a value and nothing else said about it.
defaultCookie :: Text -> Text -> Cookie
defaultCookie name value =
  Cookie
    { name
    , value
    , maxAge = Nothing
    , path = Nothing
    , domain = Nothing
    , sameSite = Nothing
    , secure = False
    }

-- | Every cookie the document can see. Cookies the browser marked @HttpOnly@
-- are not among them, which is the point of that flag.
getCookies :: (MonadIO m) => HTMLDocument -> m (Map Text Text)
getCookies doc = parseCookies <$> HTMLDocument.cookie doc

-- | One cookie by name.
getCookie :: (MonadIO m) => Text -> HTMLDocument -> m (Maybe Text)
getCookie key doc = M.lookup key <$> getCookies doc

-- | Add a cookie, or replace the one of the same name.
setCookie :: (MonadIO m) => Cookie -> HTMLDocument -> m ()
setCookie c = HTMLDocument.setCookie (renderCookie c)

-- | Remove a cookie, by writing it with an age of nothing.
--
-- A cookie is only replaced by a write that matches its path and domain, so
-- one set with either needs the same here.
deleteCookie :: (MonadIO m) => Cookie -> HTMLDocument -> m ()
deleteCookie c = setCookie c {value = "", maxAge = Just 0}

-- | Read the browser's @name=value; name=value@ into the pairs it stands for.
parseCookies :: Text -> Map Text Text
parseCookies =
  M.fromList
    . mapMaybe pair
    . T.split (== ';')
  where
    pair chunk = case T.breakOn "=" (T.strip chunk) of
      (key, rest)
        | T.null key -> Nothing
        | Just v <- T.stripPrefix "=" rest -> Just (decodeComponent key, decodeComponent v)
        -- A cookie may be a bare name, with no value at all.
        | otherwise -> Just (decodeComponent key, "")

-- | Write one cookie the way @document.cookie@ takes it.
renderCookie :: Cookie -> Text
renderCookie Cookie {..} =
  T.intercalate "; "
    $ (encodeComponent name <> "=" <> encodeComponent value)
    : catMaybes
      [ ("Max-Age=" <>) . show <$> maxAge
      , ("Path=" <>) <$> path
      , ("Domain=" <>) <$> domain
      , ("SameSite=" <>) . renderSameSite <$> sameSite
      , "Secure" <$ guard secure
      ]
  where
    renderSameSite = \case
      Strict -> "Strict"
      Lax -> "Lax"
      None -> "None"

-- | Percent-encode everything a cookie string cannot carry as itself.
encodeComponent :: Text -> Text
encodeComponent = T.concatMap escape
  where
    escape c
      | unreserved c = T.singleton c
      | otherwise = foldMap hex $ BS.unpack $ TE.encodeUtf8 $ T.singleton c

    unreserved c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-._~" :: [Char])

    hex byte = T.pack ['%', hexDigit (byte `div` 16), hexDigit (byte `mod` 16)]

    hexDigit d
      | d < 10 = chr (ord '0' + fromIntegral d)
      | otherwise = chr (ord 'A' + fromIntegral d - 10)

-- | Undo 'encodeComponent'. Anything that is not a valid escape is left as it
-- is: a cookie written by something else is still worth reading.
decodeComponent :: Text -> Text
decodeComponent = TE.decodeUtf8Lenient . BS.pack . go . T.unpack
  where
    go = \case
      '%' : hi : lo : rest
        | Just h <- digit hi
        , Just l <- digit lo ->
            fromIntegral (h * 16 + l) : go rest
      c : rest -> BS.unpack (TE.encodeUtf8 (T.singleton c)) <> go rest
      [] -> []

    digit c
      | isDigit c = Just (ord c - ord '0')
      | c >= 'a' && c <= 'f' = Just (ord c - ord 'a' + 10)
      | c >= 'A' && c <= 'F' = Just (ord c - ord 'A' + 10)
      | otherwise = Nothing
