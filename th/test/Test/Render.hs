{-# LANGUAGE GADTs #-}

-- | Halogen 'HTML' as text, for comparing what a quote built with what the
-- same markup written by hand builds: the two must render alike.
module Test.Render
  ( render
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Halogen.HTML.Core (HTML (..))
import Halogen.VDom.DOM.Monad.Class (PropValue (..))
import Halogen.VDom.DOM.Prop (Prop (..))
import Halogen.VDom.Types (ElemName (..), Namespace (..), VDom (..))
import Web.Event.Event (EventType (..))
import Web.HTML.Common (AttrName (..), PropName (..))

render :: HTML w i -> Text
render (HTML v) = go v
  where
    go = \case
      Text t -> t
      Elem ns (ElemName n) ps kids -> "<" <> spaced ns n <> mconcat (map prop ps) <> ">" <> mconcat (map go kids) <> "</" <> n <> ">"
      Keyed _ (ElemName n) ps kids -> "<" <> n <> mconcat (map prop ps) <> ">" <> mconcat (map (go . snd) kids) <> "</" <> n <> ">"
      Widget _ -> "<widget>"
      Grafted _ -> "<grafted>"
    prop = \case
      Attribute _ (AttrName a) t -> " " <> a <> "=\"" <> t <> "\""
      Property (PropName p) val -> " ." <> p <> "=" <> scalar val
      Handler (EventType e) _ -> " @" <> e
      Ref _ -> " ref"
    scalar :: PropValue a -> Text
    scalar = \case
      IntProp i -> T.pack (show (toInteger i))
      NumProp d -> T.pack (show d)
      BoolProp b -> T.pack (show b)
      TxtProp t -> quote t
      ViaTxtProp f a -> quote (f a)
    quote t = "\"" <> t <> "\""
    -- An element in a namespace (SVG's) shows it.
    spaced ns n = maybe n (\(Namespace u) -> "{" <> u <> "}" <> n) ns
