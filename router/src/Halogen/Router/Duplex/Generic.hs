{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- The OnlyConstructors constraint on 'sum' is a check, not something the
-- codec is built from: it rejects a label that names no constructor at the
-- call site, and so GHC calls it redundant.

-- | The codec of a sum type, from one codec per constructor.
--
-- PureScript writes @sum { \"Home\": noArgs, \"Post\": \"post\" / int segment }@,
-- a record whose labels are the constructors' names. Here the labels are
-- type-level strings too, written with @OverloadedLabels@, each paired with
-- its codec by '.=' and the pairs joined by '.|':
--
-- > {-# LANGUAGE OverloadedLabels #-}
-- > data Route = Home | Post Int | Tag Text
-- >   deriving stock (Generic)
-- >
-- > route :: RouteDuplex' Route
-- > route =
-- >   root $ sum $
-- >     #Home .= noArgs
-- >       .| #Post .= path "post" (int segment)
-- >       .| #Tag .= path "tag" segment
--
-- A constructor with no arguments takes a @'RouteDuplex'' ()@ ('noArgs'),
-- one with an argument a codec of that argument. A constructor with more
-- than one argument has none: give it a record, or a tuple, as its one
-- argument, as in PureScript. A constructor with no codec, or with a codec
-- of the wrong type, is a type error naming it, and so is a label that names
-- no constructor.
--
-- The list is an ordinary heterogeneous list rather than a row-types
-- record: a record is a hash map underneath, and the JavaScript backend
-- cannot hash its labels (hashable's C is not compiled there).
--
-- Printing uses the value's constructor's codec. Parsing tries them in the
-- order of the constructors and takes the first that reads the whole path
-- ('Halogen.Router.Duplex.parse'), so the order matters only between two
-- that both read an address; the order of the list does not matter at all.
module Halogen.Router.Duplex.Generic
  ( sum
  , Codecs
  , Label (..)
  , (.=)
  , (.|)
  , GRouteSum
  , GRouteArgs
  , HasCodec
  )
where

import Data.Type.Bool (If)
import Data.Type.Equality (type (~))
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Halogen.Router.Duplex (RouteDuplex (..), RouteDuplex', RouteParser, RoutePrinter)
import Protolude hiding (TypeError, sum)

-- | A constructor's name, as @#Name@.
data Label (name :: Symbol) = Label

instance (name ~ name') => IsLabel name (Label name') where
  fromLabel = Label

-- | Codecs, each under the name of the constructor it is for.
data Codecs (cs :: [(Symbol, Type)]) where
  Nil :: Codecs '[]
  Cons :: RouteDuplex' a -> Codecs cs -> Codecs ('(name, a) ': cs)

infix 7 .=

infixr 5 .|

-- | The codec of the constructor with this name.
(.=) :: Label name -> RouteDuplex' a -> Codecs '[ '(name, a)]
_ .= codec = Cons codec Nil

-- | Both lists of codecs.
(.|) :: Codecs xs -> Codecs ys -> Codecs (xs ++ ys)
Nil .| ys = ys
Cons c xs .| ys = Cons c (xs .| ys)

-- | The codec of a sum type, from a codec for each constructor, under the
-- constructor's name.
sum
  :: forall a cs
   . (Generic a, GRouteSum (Rep a) cs, OnlyConstructors (Names cs) (ConstructorNames (Rep a)))
  => Codecs cs
  -> RouteDuplex' a
sum codecs = RouteDuplex (gprint codecs . from) (to <$> gparse codecs)

-- | The constructors of a generic representation, and their codecs.
class GRouteSum (rep :: Type -> Type) (cs :: [(Symbol, Type)]) where
  gprint :: Codecs cs -> rep x -> RoutePrinter
  gparse :: Codecs cs -> RouteParser (rep x)

instance (GRouteSum f cs) => GRouteSum (M1 D meta f) cs where
  gprint codecs (M1 x) = gprint codecs x
  gparse codecs = M1 <$> gparse codecs

instance (GRouteSum f cs, GRouteSum g cs) => GRouteSum (f :+: g) cs where
  gprint codecs = \case
    L1 x -> gprint codecs x
    R1 y -> gprint codecs y
  gparse codecs = (L1 <$> gparse codecs) <|> (R1 <$> gparse codecs)

instance
  (GRouteArgs f a, HasCodec name cs a)
  => GRouteSum (M1 C ('MetaCons name fixity strict) f) cs
  where
  gprint codecs (M1 x) = (codecOf (Label @name) codecs).printer (toArgs x)
  gparse codecs = M1 . fromArgs <$> (codecOf (Label @name) codecs).parser

-- | The codec under this name.
class HasCodec (name :: Symbol) (cs :: [(Symbol, Type)]) a where
  codecOf :: Label name -> Codecs cs -> RouteDuplex' a

instance
  (TypeError ('Text "The constructor " ':<>: 'ShowType name ':<>: 'Text " has no route codec"))
  => HasCodec name '[] a
  where
  codecOf _ = panic "unreachable"

instance (Here (name == n) name ('(n, b) ': cs) a) => HasCodec name ('(n, b) ': cs) a where
  codecOf = codecHere (Proxy @(name == n))

-- | Whether the first codec is the one, and so where to look.
class Here (found :: Bool) (name :: Symbol) (cs :: [(Symbol, Type)]) a where
  codecHere :: Proxy found -> Label name -> Codecs cs -> RouteDuplex' a

instance (a ~ b) => Here 'True name ('(n, b) ': cs) a where
  codecHere _ _ (Cons codec _) = codec

instance (HasCodec name cs a) => Here 'False name ('(n, b) ': cs) a where
  codecHere _ name (Cons _ rest) = codecOf name rest

-- | A constructor's arguments as the one value its codec handles.
class GRouteArgs (f :: Type -> Type) a | f -> a where
  toArgs :: f x -> a
  fromArgs :: a -> f x

instance GRouteArgs U1 () where
  toArgs U1 = ()
  fromArgs () = U1

instance GRouteArgs (M1 S meta (K1 R a)) a where
  toArgs (M1 (K1 a)) = a
  fromArgs = M1 . K1

instance
  (TypeError ('Text "A route's constructor takes at most one argument: give it a record, or a tuple, instead"))
  => GRouteArgs (f :*: g) ()
  where
  toArgs = panic "unreachable"
  fromArgs = panic "unreachable"

type family Names (cs :: [(Symbol, Type)]) :: [Symbol] where
  Names '[] = '[]
  Names ('(name, a) ': cs) = name ': Names cs

type family ConstructorNames (rep :: Type -> Type) :: [Symbol] where
  ConstructorNames (M1 D meta f) = ConstructorNames f
  ConstructorNames (f :+: g) = ConstructorNames f ++ ConstructorNames g
  ConstructorNames (M1 C ('MetaCons name fixity strict) f) = '[name]
  ConstructorNames V1 = '[]

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Every label names a constructor: a codec for a constructor that is not
-- there is a mistake, a misspelt name most likely.
type family OnlyConstructors (labels :: [Symbol]) (names :: [Symbol]) :: Constraint where
  OnlyConstructors '[] names = ()
  OnlyConstructors (l ': ls) names =
    ( If
        (Elem l names)
        (() :: Constraint)
        (TypeError ('Text "The route codec " ':<>: 'ShowType l ':<>: 'Text " names no constructor"))
    , OnlyConstructors ls names
    )

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs
