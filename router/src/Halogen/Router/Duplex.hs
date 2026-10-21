-- | Routes as codecs: one value that both reads an address into a route and
-- writes a route back as an address, so the two cannot drift apart. A port of
-- the core of purescript-routing-duplex.
--
-- A 'RouteDuplex' is a printer and a parser built side by side from the same
-- combinators. An address is taken apart into path segments, query
-- parameters and a fragment ('RouteState'); a printer adds to those, a parser
-- consumes them.
--
-- > data Route = Home | Post Int | Search Text (Maybe Int)
-- >
-- > route :: RouteDuplex' Route
-- > route = root $ sum $
-- >      #Home .== noArgs
-- >   .+ #Post .== path "post" (int segment)
-- >   .+ ...
--
-- ('Halogen.Router.Duplex.Generic.sum' derives the alternatives of a sum
-- type.) Several values of a constructor's record are built with the
-- 'Applicative' instance and 'lmap', which says which part of the route each
-- one prints:
--
-- > data Search = Search {query :: Text, page :: Maybe Int}
-- >
-- > search :: RouteDuplex' Search
-- > search = path "search" $
-- >   Search <$> lmap (.query) (param "q") <*> lmap (.page) (optional (int (param "page")))
--
-- Two departures from the PureScript library. A route is read only when the
-- whole path has been consumed: @\/post\/1\/extra@ is not @Post 1@, so a route
-- that is a prefix of another does not shadow it, whichever is listed first,
-- and 'end' is needed only inside a route. And the parser backtracks: of the
-- ways to read an address, the first that consumes all of it wins, so
-- 'optional' and 'many' give back what a later part needs.
module Halogen.Router.Duplex
  ( -- * Codecs
    RouteDuplex (..)
  , RouteDuplex'
  , parse
  , print
  , RouteError (..)
  , lmap

    -- * Paths
  , root
  , path
  , prefix
  , suffix
  , segment
  , rest
  , end
  , noArgs

    -- * Query parameters
  , param
  , flag

    -- * Values
  , as
  , int
  , string
  , boolean

    -- * Repetition
  , optional
  , many
  , many1

    -- * Addresses taken apart
  , RouteState (..)
  , parseAddress
  , printAddress
  , RouteParser
  , RoutePrinter
  , runParser
  , runPrinter
  , encodeComponent
  , decodeComponent
  )
where

import Data.ByteString qualified as B
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Numeric (showHex)
import Protolude hiding (many, optional, print)

-- | An address taken apart: the path's segments, decoded, in order (an
-- address that starts with @\/@ has an empty first one, which is what 'root'
-- reads), the query's parameters, decoded, and the fragment as written,
-- without its @#@.
data RouteState = RouteState
  { segments :: [Text]
  , params :: [(Text, Text)]
  , fragment :: Text
  }
  deriving stock (Eq, Show)

-- | Why an address is not a route.
data RouteError
  = -- | This segment was wanted and that one found.
    Expected Text Text
  | -- | The path went on, with this segment, past the end of the route.
    ExpectedEndOfPath Text
  | -- | The route needs this query parameter.
    MissingParam Text
  | -- | The route needs another segment.
    EndOfPath
  | -- | 'as' did not accept a value, for this reason.
    Invalid Text
  deriving stock (Eq, Show)

-- | What a printer adds to an address. Printers compose left to right: the
-- first one's segments come first.
newtype RoutePrinter = RoutePrinter (RouteState -> RouteState)

instance Semigroup RoutePrinter where
  RoutePrinter f <> RoutePrinter g = RoutePrinter (g . f)

instance Monoid RoutePrinter where
  mempty = RoutePrinter identity

runPrinter :: RoutePrinter -> RouteState
runPrinter (RoutePrinter f) = f (RouteState [] [] "")

-- | A failure, and how many segments were left when it happened: of the
-- ways an address could not be read, the one that got furthest says best
-- what is wrong with it.
data Failure = Failure Int RouteError

furthest :: Maybe Failure -> Maybe Failure -> Maybe Failure
furthest (Just a@(Failure n _)) (Just b@(Failure m _)) = Just (if m < n then b else a)
furthest a b = a <|> b

-- | Every way of reading the address so far, in order of preference, with
-- what each leaves; and the furthest failure met on the way.
data Outcome a = Outcome [(a, RouteState)] (Maybe Failure)

-- | Reads a route out of an address. 'Alternative' tries both sides and
-- keeps every reading, the left's first.
newtype RouteParser a = RouteParser (RouteState -> Outcome a)

runParser :: RouteParser a -> RouteState -> [(a, RouteState)]
runParser (RouteParser p) s = let Outcome found _ = p s in found

instance Functor RouteParser where
  fmap f (RouteParser p) = RouteParser $ \s ->
    let Outcome found failure = p s in Outcome [(f a, s') | (a, s') <- found] failure

instance Applicative RouteParser where
  pure a = RouteParser $ \s -> Outcome [(a, s)] Nothing
  (<*>) = ap

instance Monad RouteParser where
  RouteParser p >>= k = RouteParser $ \s ->
    let Outcome found failure = p s
        nexts = [let RouteParser q = k a in q s' | (a, s') <- found]
     in Outcome
          (concat [rs | Outcome rs _ <- nexts])
          (foldl' furthest failure [f | Outcome _ f <- nexts])

instance Alternative RouteParser where
  empty = RouteParser $ \_ -> Outcome [] Nothing
  RouteParser p <|> RouteParser q = RouteParser $ \s ->
    let Outcome a fa = p s
        Outcome b fb = q s
     in Outcome (a <> b) (furthest fa fb)

failWith :: RouteError -> RouteParser a
failWith e = RouteParser $ \s -> Outcome [] (Just (Failure (length s.segments) e))

-- | The left's readings if it has any, otherwise the right's.
ifNone :: RouteParser a -> RouteParser a -> RouteParser a
ifNone (RouteParser p) (RouteParser q) = RouteParser $ \s -> case p s of
  Outcome [] fa -> let Outcome b fb = q s in Outcome b (furthest fa fb)
  found -> found

-- | A printer and a parser, built together. @i@ is what the printer takes,
-- @o@ what the parser gives; a codec for a route type has both the same,
-- 'RouteDuplex''. They differ while a route is being assembled from its
-- parts: see 'lmap'.
data RouteDuplex i o = RouteDuplex
  { printer :: i -> RoutePrinter
  , parser :: RouteParser o
  }

type RouteDuplex' a = RouteDuplex a a

instance Functor (RouteDuplex i) where
  fmap f (RouteDuplex enc dec) = RouteDuplex enc (fmap f dec)

-- | Side by side: each part prints its share of the same value and parses
-- one argument of the constructor.
instance Applicative (RouteDuplex i) where
  pure a = RouteDuplex (const mempty) (pure a)
  RouteDuplex encF decF <*> RouteDuplex encA decA = RouteDuplex (\i -> encF i <> encA i) (decF <*> decA)

-- | Which part of a value a codec prints.
lmap :: (j -> i) -> RouteDuplex i o -> RouteDuplex j o
lmap f (RouteDuplex enc dec) = RouteDuplex (enc . f) dec

-- | Read an address (a path, then @?@ and a query, then @#@ and a fragment,
-- the last two optional) as a route, consuming the whole path.
parse :: RouteDuplex i o -> Text -> Either RouteError o
parse (RouteDuplex _ (RouteParser p)) address =
  case p (parseAddress address) of
    Outcome found failure -> case [a | (a, s) <- found, null s.segments] of
      a : _ -> Right a
      [] ->
        let leftover = [Failure (length s.segments) (ExpectedEndOfPath next) | (_, s) <- found, next : _ <- [s.segments]]
         in case foldl' furthest failure (map Just leftover) of
              Just (Failure _ e) -> Left e
              Nothing -> Left EndOfPath

-- | Write a route as an address.
print :: RouteDuplex i o -> i -> Text
print (RouteDuplex enc _) = printAddress . runPrinter . enc

-- | Take an address apart. @\/@ alone is one empty segment, the root.
parseAddress :: Text -> RouteState
parseAddress address = RouteState {segments, params, fragment = T.drop 1 afterHash}
  where
    (beforeHash, afterHash) = T.breakOn "#" address
    (pathPart, query) = T.breakOn "?" beforeHash
    segments = case pathPart of
      "" -> []
      "/" -> [""]
      _ -> map decodeComponent (T.splitOn "/" pathPart)
    params =
      [ (decodeComponent k, decodeComponent (T.drop 1 v))
      | pair <- T.splitOn "&" (T.drop 1 query)
      , not (T.null pair)
      , let (k, v) = T.breakOn "=" pair
      ]

-- | Put an address together; the inverse of 'parseAddress'.
printAddress :: RouteState -> Text
printAddress address = pathPart <> query <> hashPart
  where
    RouteState {segments, params, fragment} = address
    pathPart = case segments of
      [""] -> "/"
      _ -> T.intercalate "/" (map encodeComponent segments)
    query = case params of
      [] -> ""
      _ -> "?" <> T.intercalate "&" [encodeComponent k <> (if T.null v then "" else "=" <> encodeComponent v) | (k, v) <- params]
    hashPart = if T.null fragment then "" else "#" <> fragment

-- | Percent-encode as JavaScript's @encodeURIComponent@ does: everything but
-- letters, digits and @-_.!~*'()@, as the bytes of its UTF-8.
encodeComponent :: Text -> Text
encodeComponent = T.concatMap encodeChar
  where
    encodeChar c
      | isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("-_.!~*'()" :: [Char]) = T.singleton c
      | otherwise = foldMap hex (B.unpack (TE.encodeUtf8 (T.singleton c)))
    hex b = "%" <> T.toUpper (T.justifyRight 2 '0' (toS (showHex b "")))

-- | Undo 'encodeComponent', or any percent-encoding: a @%@ not followed by
-- two hexadecimal digits stays as it is, and bytes that are not UTF-8 become
-- the replacement character.
decodeComponent :: Text -> Text
decodeComponent txt
  | "%" `T.isInfixOf` txt = TE.decodeUtf8With lenientDecode (B.pack (go (B.unpack (TE.encodeUtf8 txt))))
  | otherwise = txt
  where
    go (37 : a : b : more) | Just x <- hexDigit a, Just y <- hexDigit b = x * 16 + y : go more
    go (c : more) = c : go more
    go [] = []
    hexDigit w
      | w >= 48 && w <= 57 = Just (w - 48)
      | w >= 65 && w <= 70 = Just (w - 55)
      | w >= 97 && w <= 102 = Just (w - 87)
      | otherwise = Nothing

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

putSegment :: Text -> RoutePrinter
putSegment s = RoutePrinter $ \st -> st {segments = st.segments <> [s]}

takeSegment :: RouteParser Text
takeSegment = RouteParser $ \s -> case s.segments of
  next : more -> Outcome [(next, s {segments = more})] Nothing
  [] -> Outcome [] (Just (Failure 0 EndOfPath))

expect :: Text -> RouteParser ()
expect wanted = RouteParser $ \s -> case s.segments of
  next : more
    | next == wanted -> Outcome [((), s {segments = more})] Nothing
    | otherwise -> Outcome [] (Just (Failure (length s.segments) (Expected wanted next)))
  [] -> Outcome [] (Just (Failure 0 EndOfPath))

-- | A fixed segment before what the codec reads.
prefix :: Text -> RouteDuplex i o -> RouteDuplex i o
prefix s (RouteDuplex enc dec) = RouteDuplex (\a -> putSegment s <> enc a) (expect s *> dec)

-- | A fixed segment after what the codec reads.
suffix :: RouteDuplex i o -> Text -> RouteDuplex i o
suffix (RouteDuplex enc dec) s = RouteDuplex (\a -> enc a <> putSegment s) (dec <* expect s)

-- | Fixed segments, written as a path: @path \"blog\/posts\"@ is
-- @prefix \"blog\" . prefix \"posts\"@. A trailing @\/@ is an empty last
-- segment, so @path \"posts\/\" noArgs@ is @posts\/@ and not @posts@.
path :: Text -> RouteDuplex i o -> RouteDuplex i o
path p codec = foldr prefix codec (T.splitOn "/" p)

-- | The address starts with @\/@: an absolute path.
root :: RouteDuplex i o -> RouteDuplex i o
root = path ""

-- | The path ends here.
end :: RouteDuplex i o -> RouteDuplex i o
end (RouteDuplex enc dec) = RouteDuplex enc (dec <* atEnd)
  where
    atEnd = RouteParser $ \s -> case s.segments of
      [] -> Outcome [((), s)] Nothing
      next : _ -> Outcome [] (Just (Failure (length s.segments) (ExpectedEndOfPath next)))

-- | One segment, whatever it is.
segment :: RouteDuplex' Text
segment = RouteDuplex putSegment takeSegment

-- | Every segment that is left.
rest :: RouteDuplex' [Text]
rest = RouteDuplex (foldMap putSegment) $ RouteParser $ \s -> Outcome [(s.segments, s {segments = []})] Nothing

-- | Nothing at all: the codec of a constructor with no arguments.
noArgs :: RouteDuplex' ()
noArgs = pure ()

--------------------------------------------------------------------------------
-- Query parameters
--------------------------------------------------------------------------------

-- | The value of a query parameter. Parameters can come in any order.
param :: Text -> RouteDuplex' Text
param name = RouteDuplex putParam takeParam
  where
    putParam v = RoutePrinter $ \st -> st {params = st.params <> [(name, v)]}
    takeParam = RouteParser $ \s -> case lookup name s.params of
      Just v -> Outcome [(v, s)] Nothing
      Nothing -> Outcome [] (Just (Failure (length s.segments) (MissingParam name)))

-- | Whether a parameter is there at all: @?draft@ for 'True', nothing for
-- 'False'.
flag :: RouteDuplex' Text -> RouteDuplex' Bool
flag (RouteDuplex enc dec) =
  RouteDuplex (\b -> if b then enc "" else mempty) ((dec $> True) `ifNone` pure False)

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

-- | A value written as text: how it is printed, and how it is read back, or
-- why it cannot be.
as :: (s -> Text) -> (Text -> Either Text a) -> RouteDuplex' Text -> RouteDuplex s a
as printValue parseValue (RouteDuplex enc dec) =
  RouteDuplex (enc . printValue) (dec >>= either (failWith . Invalid) pure . parseValue)

-- | A whole number, in decimal.
int :: RouteDuplex' Text -> RouteDuplex' Int
int = as show $ \t -> case TR.signed TR.decimal t of
  Right (n, "") -> Right n
  _ -> Left ("not a whole number: " <> t)

-- | Text as it is: only there to say so where a route is spelled out.
string :: RouteDuplex' Text -> RouteDuplex' Text
string = identity

-- | @true@ or @false@.
boolean :: RouteDuplex' Text -> RouteDuplex' Bool
boolean = as (\b -> if b then "true" else "false") $ \case
  "true" -> Right True
  "false" -> Right False
  t -> Left ("not true or false: " <> t)

--------------------------------------------------------------------------------
-- Repetition
--------------------------------------------------------------------------------

-- | The codec's value, or nothing where it would be.
optional :: RouteDuplex i o -> RouteDuplex (Maybe i) (Maybe o)
optional (RouteDuplex enc dec) = RouteDuplex (maybe mempty enc) ((Just <$> dec) <|> pure Nothing)

-- | The codec's value, as many times as it is there (as many as it can be,
-- first). A reading that consumes no segments is not repeated.
many :: RouteDuplex i o -> RouteDuplex [i] [o]
many (RouteDuplex enc dec) = RouteDuplex (foldMap enc) (repeated dec)

-- | Like 'many', at least once.
many1 :: RouteDuplex i o -> RouteDuplex [i] [o]
many1 (RouteDuplex enc dec) = RouteDuplex (foldMap enc) ((:) <$> dec <*> repeated dec)

repeated :: RouteParser o -> RouteParser [o]
repeated (RouteParser p) = RouteParser go
  where
    go s =
      let Outcome found failure = p s
          longer = [(a, s') | (a, s') <- found, length s'.segments < length s.segments]
          nexts = [(a, go s') | (a, s') <- longer]
       in Outcome
            ([(a : as', s'') | (a, Outcome more _) <- nexts, (as', s'') <- more] <> [([], s)])
            (foldl' furthest failure [f | (_, Outcome _ f) <- nexts])
