{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | What a @{{ … }}@ among the children of an element, or among its
-- attributes, may be. The quoter splices one in through these classes, so
-- a hole takes one node, a list of them, a text, or nothing.
module Halogen.TH.Html.Children
  ( Children (..)
  , Props (..)
  , Styled (..)
  , viewBoxOf
  , unknownHtml
  , unknownSvg
  )
where

import Clay (Css)
import Clay.Render qualified as Clay
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Halogen.HTML.Core (ElemName (..), HTML)
import Halogen.HTML.Core qualified as HH
import Halogen.HTML.Elements qualified as HHE
import Halogen.HTML.Properties (IProp)
import DOM.HTML.Indexed qualified as I
import Data.Row (Empty, HasType)
import Halogen.Svg.Attributes qualified as SA
import Halogen.Svg.Elements qualified as SE
import Halogen.Svg.Indexed qualified as SI

-- | A value among the children of an element.
class Children w i a where
  children :: a -> [HTML w i]

instance (w ~ w', i ~ i') => Children w i (HTML w' i') where
  children h = [h]

instance (Children w i a) => Children w i [a] where
  children = concatMap children

instance (Children w i a) => Children w i (Maybe a) where
  children = maybe [] children

instance Children w i Text where
  children t = [HH.text t]

-- | A style among the children is a @\<style>@ element with it, so a
-- @clay@ quote is embedded with @{{ css }}@.
instance Children w i Css where
  children c = [HHE.element (ElemName "style") [] [HH.text (TL.toStrict (Clay.renderWith Clay.compact [] c))]]

instance (TypeError ('Text "A String among the children of an element: use Text, or pack it." ':$$: 'Text "A character is not a child.")) => Children w i Char where
  children = error "unreachable"

-- | A value among the attributes of an element.
class Props r i a where
  props :: a -> [IProp r i]

instance (r ~ r', i ~ i') => Props r i (IProp r' i') where
  props p = [p]

instance (Props r i a) => Props r i [a] where
  props = concatMap props

instance (Props r i a) => Props r i (Maybe a) where
  props = maybe [] props

-- | The value of @style={{ … }}@: Clay, or text, as the attribute's text.
class Styled a where
  styled :: a -> Text

instance Styled Css where
  styled = TL.toStrict . Clay.renderWith Clay.htmlInline []

instance Styled Text where
  styled = id

-- | A @viewBox@ from Haskell: its four numbers, as a tuple.
viewBoxOf :: (HasType "viewBox" Text r) => (Double, Double, Double, Double) -> IProp r i
viewBoxOf (x, y, w, h) = SA.viewBox x y w h

-- | An HTML element Halogen has no function for. Its attributes are those
-- any element has (and its handlers), so they are typed: on an open row
-- a property would be ambiguous.
unknownHtml :: ElemName -> [IProp (I.Interactive Empty) i] -> [HTML w i] -> HTML w i
unknownHtml = HHE.element

-- | The same for an SVG element ("Halogen.Svg.Elements" has no
-- @\<stop>@ or @\<linearGradient>@, for one).
unknownSvg :: ElemName -> [IProp (SI.GlobalAttributes Empty) i] -> [HTML w i] -> HTML w i
unknownSvg = SE.element
