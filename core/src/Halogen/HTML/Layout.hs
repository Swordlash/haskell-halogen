module Halogen.HTML.Layout where

import Data.Default
import HPrelude hiding ((>>))
import Halogen.HTML qualified as HH

class Layout (f :: Type -> Type -> Type) where
  data LayoutSettings f
  data LayoutConstraints f

  layout :: LayoutSettings f -> f w i -> HH.HTML w i
  layoutEnd :: f w i
  addComponent :: LayoutConstraints f -> HH.HTML w i -> f w i -> f w i

addComponent' :: (Layout f, Default (LayoutConstraints f)) => HH.HTML w i -> f w i -> f w i
addComponent' = addComponent def

layout' :: (Layout f, Default (LayoutSettings f)) => f w i -> HH.HTML w i
layout' = layout def

addComponentIf :: (Layout f) => Bool -> LayoutConstraints f -> HH.HTML w i -> f w i -> f w i
addComponentIf b c h a = if b then addComponent c h a else a

addComponentIf' :: (Layout f, Default (LayoutConstraints f)) => Bool -> HH.HTML w i -> f w i -> f w i
addComponentIf' b h a = addComponentIf b def h a

newtype LayoutM f w i = LayoutM (f w i -> f w i)

runLayoutM :: (Layout f) => LayoutSettings f -> LayoutM f w i -> HH.HTML w i
runLayoutM settings (LayoutM f) = layout settings $ f layoutEnd

end :: (Layout f) => LayoutM f w i
end = LayoutM identity

class (Layout f) => AddLayout f w i a | a -> w i where
  (>>) :: a -> LayoutM f w i -> LayoutM f w i

instance (AddLayout f w i a) => AddLayout f w i (Bool, a) where
  (b, f) >> g = if b then f >> g else g

instance (Layout f) => AddLayout f w i (LayoutConstraints f, HH.HTML w i) where
  (c, h) >> LayoutM f = LayoutM $ f . addComponent c h

instance (Layout f, Default (LayoutConstraints f)) => AddLayout f w i (HH.HTML w i) where
  html >> LayoutM f = LayoutM $ f . addComponent @f def html

instance (Layout f) => AddLayout f w i (f w i -> f w i) where
  f >> l = (True, f) >> l

infixr 1 >>

if_ :: Bool -> a -> (Bool, a)
if_ = (,)

with :: LayoutConstraints f -> HH.HTML w i -> (LayoutConstraints f, HH.HTML w i)
with = (,)

-- (>>) :: (Layout f, Default (LayoutConstraints f)) => HH.HTML w i -> LayoutM f w i -> LayoutM f w i
-- html >> LayoutM g = LayoutM $ g . addComponent' html
