-- | What a quoter would say about a quote, found without running it as a
-- splice: for the compiler plugin ("Halogen.TH.Plugin" in
-- haskell-halogen-th-plugin), or any tool that has a quote's text and its
-- place in a file.
module Halogen.TH.Check
  ( Quoter (..)
  , quoterNamed
  , quoterName
  , Complaint (..)
  , check
  )
where

import Control.Exception (Exception, throwIO, try)
import Data.Text (Text)
import Data.Text qualified as T
import Halogen.TH.Clay (cssExp)
import Halogen.TH.Haskell (Failure (..), parseExpAt, parsePatAt)
import Halogen.TH.Html (htmlExp)
import Halogen.TH.Template (Env (..))
import Language.Haskell.TH (Extension, Q, runIO, runQ)

-- | The three quoters of "Halogen.TH".
data Quoter = Hh | Hhs | Clay
  deriving (Eq, Show)

-- | A quoter by the name it is used under.
quoterNamed :: Text -> Maybe Quoter
quoterNamed = \case
  "hh" -> Just Hh
  "hhs" -> Just Hhs
  "clay" -> Just Clay
  _ -> Nothing

quoterName :: Quoter -> Text
quoterName = \case
  Hh -> "hh"
  Hhs -> "hhs"
  Clay -> "clay"

-- | What is wrong with a quote: the line of the file, the column when it
-- is known (a parse error in the Haskell of a hole has one), and the
-- message, as the quoter words it after its prefix.
data Complaint = Complaint
  { line :: Int
  , column :: Maybe Int
  , message :: Text
  }
  deriving (Eq, Show)

instance Exception Complaint

-- | A quote's text, starting at a line and column of its file, run through
-- its quoter with the module's extensions: what the quoter would fail
-- with, or nothing. Of the Q monad the quoters use only failure, so the
-- run is in IO, with the failure an exception.
check :: [Extension] -> Quoter -> (Int, Int) -> Text -> IO (Maybe Complaint)
check exts quoter (startLine, startColumn) src =
  either Just (const Nothing) <$> try (runQ (quote env (T.unpack src)))
  where
    quote = case quoter of
      Hh -> \e -> htmlExp e True
      Hhs -> \e -> htmlExp e False
      Clay -> cssExp
    env =
      Env
        { expression = \origin s -> parsed origin (parseExpAt exts (inFile origin) s)
        , pattern = \origin s -> parsed origin (parsePatAt exts (inFile origin) s)
        , report = \ln msg -> complain (Complaint (fileLine ln) Nothing msg)
        , startColumn
        }
    -- Lines are the template's; a hole's columns are the file's already.
    fileLine ln = if ln > 0 then startLine + ln - 1 else startLine
    inFile (ln, col) = (fileLine ln, col)
    parsed (ln, _) = \case
      Right x -> pure x
      Left (Failure l c msg)
        | l > 0 -> complain (Complaint l (Just c) msg)
        | otherwise -> complain (Complaint (fileLine ln) Nothing msg)
    complain :: Complaint -> Q a
    complain = runIO . throwIO
