-- | A lesson's text, as HTML.
module Learn.View (blocks, inlines) where

import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Learn.Markdown (Block (..), Inline (..))
import Protolude

blocks :: [Block] -> [HH.HTML w i]
blocks = map block

block :: Block -> HH.HTML w i
block = \case
  Heading level content -> heading level (inlines content)
  Paragraph content -> HH.p_ (inlines content)
  CodeBlock language code ->
    HH.pre_ [HH.code [HP.class_ (HH.ClassName ("language-" <> lang)) | Just lang <- [language]] [HH.text code]]
  Bullets items -> HH.ul_ [HH.li_ (inlines item) | item <- items]
  Numbered items -> HH.ol_ [HH.li_ (inlines item) | item <- items]
  Quote inner -> HH.blockquote_ (blocks inner)
  where
    heading = \case
      1 -> HH.h1_
      2 -> HH.h2_
      _ -> HH.h3_

inlines :: [Inline] -> [HH.HTML w i]
inlines = map $ \case
  Plain t -> HH.text t
  Code t -> HH.code_ [HH.text t]
  Strong xs -> HH.strong_ (inlines xs)
  Emphasis xs -> HH.em_ (inlines xs)
  Link address xs -> HH.a [HP.href address] (inlines xs)
