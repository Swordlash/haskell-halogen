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
--
-- An @\<svg>@ in an @hh@ quote, and all of an @svg@ quote, is SVG: its
-- elements and attributes are those of "Halogen.Svg" (@\<path d={{ cmds }}>@,
-- @\<circle cx={{ x }} r="4">@, @viewBox={{ (0, 0, w, h) }}@), its class an
-- attribute, as an SVG element needs.
module Halogen.TH
  ( hh
  , hhs
  , svg
  , svgs
  , clay
  , Children (..)
  , Props (..)
  , Styled (..)
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Halogen.TH.Clay (cssExp)
import Halogen.TH.Haskell (Failure (..), parseExpAt, parsePatAt)
import Halogen.TH.Html (Dialect (..), htmlExp)
import Halogen.TH.Html.Children (Children (..), Props (..), Styled (..))
import Halogen.TH.Template (Env (..))
import Language.Haskell.TH (Exp, Q, loc_filename, loc_start, location)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (extsEnabled)

-- | One element with Haskell in it, as @HTML w i@.
hh :: QuasiQuoter
hh = quoter "hh" (\env -> htmlExp Html env True)

-- | Any number of nodes with Haskell in them, as @[HTML w i]@.
hhs :: QuasiQuoter
hhs = quoter "hhs" (\env -> htmlExp Html env False)

-- | One SVG element with Haskell in it, as @HTML w i@: what goes inside an
-- @\<svg>@ (which in an @hh@ quote is SVG already), written on its own.
svg :: QuasiQuoter
svg = quoter "svg" (\env -> htmlExp Svg env True)

-- | Any number of SVG nodes with Haskell in them, as @[HTML w i]@.
svgs :: QuasiQuoter
svgs = quoter "svgs" (\env -> htmlExp Svg env False)

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
      file = T.pack (loc_filename loc)
      at ln
        | ln > 0 = file <> ":" <> T.pack (show (startLine + ln - 1)) <> ": "
        | otherwise = ""
      report ln msg = fail (T.unpack (who <> ": " <> at ln <> msg))
      -- A hole's columns are the file's already; its lines are the template's.
      inFile (ln, col) = (startLine + ln - 1, col)
      -- GHC parses the code from its place in the file, so a failure is
      -- already placed there.
      parsed origin = \case
        Right x -> pure x
        Left (Failure l c msg)
          | l > 0 -> fail (T.unpack (who <> ": " <> file <> ":" <> T.pack (show l) <> ": column " <> T.pack (show c) <> ": " <> msg))
          | otherwise -> report (fst origin) msg
  pure
    Env
      { expression = \origin src -> parsed origin (parseExpAt exts (inFile origin) src)
      , pattern = \origin src -> parsed origin (parsePatAt exts (inFile origin) src)
      , report
      , startColumn
      }
