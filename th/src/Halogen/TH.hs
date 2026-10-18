-- | HTML and CSS as they are written, with Haskell inside, for Halogen and
-- Clay.
--
-- @
-- view st = [hh|
--   <div class="counter {{ if st.on then "on" else "" }}">
--     <button onClick={{ const Decrement }}>−</button>
--     <span class="count">{{ show st.count }}</span>
--     <button onClick={{ const Increment }}>+</button>
--     {% if st.count > 9 %}<em>That is a lot.</em>{% end %}
--     <ul>{% for (i, t) in zip [1 :: Int ..] st.items %}<li>{{ show i }}. {{ t }}</li>{% end %}</ul>
--   </div>
-- |]
--
-- style = [clay|
--   .counter { display: flex; gap: 8px; color: {{ ink }};
--     &.on { font-weight: bold; }
--     > .count { min-width: 2em; }
--     @media (max-width: 600px) { flex-direction: column; }
--   }
-- |]
-- @
--
-- A @{{ … }}@ is any Haskell expression, parsed with the extensions of the
-- module it is in. As a child it is a node, a list of nodes or a 'Text'
-- (see 'Children'); as an attribute value, what the property takes; among
-- the attributes, a property or a list of them (see 'Props'); in a style,
-- a value (anything Clay can render, by 'Clay.Val') or a rule. A
-- @{% if c %} … {% elif c %} … {% else %} … {% end %}@, a
-- @{% for pattern in list %} … {% end %}@ and a @{% let pattern = e %}@
-- (over the rest of its sequence) work among children, attributes and
-- rules alike. A @{# … #}@ is a comment.
module Halogen.TH
  ( hh
  , hhs
  , clay
  , Children (..)
  , Props (..)
  , Styled (..)
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Halogen.TH.Clay (cssExp)
import Halogen.TH.Haskell (Failure (..), parseExp, parsePat)
import Halogen.TH.Html (htmlExp)
import Halogen.TH.Html.Children (Children (..), Props (..), Styled (..))
import Halogen.TH.Template (Env (..))
import Language.Haskell.TH (Exp, Q, location, loc_filename, loc_start)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (extsEnabled)

-- | One element with Haskell in it, as @HTML w i@.
hh :: QuasiQuoter
hh = quoter "hh" (\env -> htmlExp env True)

-- | Any number of nodes with Haskell in them, as @[HTML w i]@.
hhs :: QuasiQuoter
hhs = quoter "hhs" (\env -> htmlExp env False)

-- | Rules with Haskell in them, as 'Clay.Css'.
clay :: QuasiQuoter
clay = quoter "clay" cssExp

quoter :: Text -> (Env -> String -> Q Exp) -> QuasiQuoter
quoter who run =
  QuasiQuoter
    { quoteExp = \src -> environment who >>= (`run` src)
    , quotePat = other
    , quoteType = other
    , quoteDec = other
    }
  where
    other _ = fail (T.unpack who <> " quotes an expression only")

-- | The Haskell inside the quote is parsed with the quoting module's
-- extensions; an error names the file, the line and the column, counted
-- in the file: the quote's text starts where the quote opens.
environment :: Text -> Q Env
environment who = do
  exts <- extsEnabled
  loc <- location
  let (startLine, startColumn) = loc_start loc
      at ln
        | ln > 0 = T.pack (loc_filename loc) <> ":" <> T.pack (show (startLine + ln - 1)) <> ": "
        | otherwise = ""
      report ln msg = T.unpack (who <> ": " <> at ln <> msg)
      -- The first line of the quote's text begins after its opening.
      fileColumn ln col = if ln == 1 then startColumn + col - 1 else col
      parsed (ln, col) = \case
        Right x -> pure x
        Left (Failure l c msg)
          | l == 1 -> fail (report ln ("column " <> T.pack (show (fileColumn ln col + c - 1)) <> ": " <> msg))
          | l > 1 -> fail (report (ln + l - 1) ("column " <> T.pack (show c) <> ": " <> msg))
          | otherwise -> fail (report ln msg)
  pure
    Env
      { expression = \at' src -> parsed at' (parseExp exts src)
      , pattern = \at' src -> parsed at' (parsePat exts src)
      , report
      }
