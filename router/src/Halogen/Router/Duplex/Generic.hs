{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- The OnlyConstructors constraint on 'sum' is a check, not something the
-- codec is built from: it rejects a label that names no constructor at the
-- call site, and so GHC calls it redundant.

-- | The codec of a sum type, from one codec per constructor.
--
-- PureScript writes @sum { \"Home\": noArgs, \"Post\": \"post\" / int segment }@,
-- a record whose labels are the constructors' names. Here the record is a
-- row-types one, whose labels are type-level strings and so can be the
-- constructors' names too:
--
-- > {-# LANGUAGE OverloadedLabels #-}
-- > data Route = Home | Post Int | Tag Text
-- >   deriving stock (Generic)
-- >
-- > route :: RouteDuplex' Route
-- > route =
-- >   root $ sum $
-- >     #Home .== noArgs
-- >       .+ #Post .== path "post" (int segment)
-- >       .+ #Tag .== path "tag" segment
--
-- A constructor with no arguments takes a @'RouteDuplex'' ()@ ('noArgs'),
-- one with an argument a codec of that argument. A constructor with more
-- than one argument has none: give it a record, or a tuple, as its one
-- argument, as in PureScript. A constructor with no codec, or with a codec
-- of the wrong type, is a type error naming it, and so is a label that names
-- no constructor.
--
-- A record looks its fields up by hashing their labels, as 'Data.Text.Text',
-- with hashable. On the JavaScript backend hashable's hashing is C that GHC
-- does not compile there; haskell-halogen-core links a port of it
-- (@jsbits\/polyfills.js@) into every program, so records work there too.
--
-- Printing uses the value's constructor's codec. Parsing tries them in the
-- order of the constructors and takes the first that reads the whole path
-- ('Halogen.Router.Duplex.parse'), so the order matters only between two
-- that both read an address; the order of the record does not matter at all.
module Halogen.Router.Duplex.Generic
  ( sum
  , GRouteSum
  , GRouteArgs
  , HasCodec
  )
where

import Data.Row.Internal (Label (..), Labels, Row)
import Data.Row.Records (HasType, Rec, (.!))
import Data.Type.Bool (If)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Halogen.Router.Duplex (RouteDuplex (..), RouteDuplex', RouteParser, RoutePrinter)
import Protolude hiding (TypeError, sum)

-- | The codec of a sum type, from a record of a codec for each constructor,
-- under the constructor's name.
sum
  :: forall a (r :: Row Type)
   . (Generic a, GRouteSum (Rep a) r, OnlyConstructors (Labels r) (ConstructorNames (Rep a)))
  => Rec r
  -> RouteDuplex' a
sum codecs = RouteDuplex (gprint codecs . from) (to <$> gparse codecs)

-- | The constructors of a generic representation, and their codecs in a
-- record.
class GRouteSum (rep :: Type -> Type) (r :: Row Type) where
  gprint :: Rec r -> rep x -> RoutePrinter
  gparse :: Rec r -> RouteParser (rep x)

instance (GRouteSum f r) => GRouteSum (M1 D meta f) r where
  gprint codecs (M1 x) = gprint codecs x
  gparse codecs = M1 <$> gparse codecs

instance (GRouteSum f r, GRouteSum g r) => GRouteSum (f :+: g) r where
  gprint codecs = \case
    L1 x -> gprint codecs x
    R1 y -> gprint codecs y
  gparse codecs = (L1 <$> gparse codecs) <|> (R1 <$> gparse codecs)

instance
  (GRouteArgs f a, HasCodec name r a)
  => GRouteSum (M1 C ('MetaCons name fixity strict) f) r
  where
  gprint codecs (M1 x) = (codecOf (Label @name) codecs).printer (toArgs x)
  gparse codecs = M1 . fromArgs <$> (codecOf (Label @name) codecs).parser

-- | The record's codec for the constructor with this name. A constructor
-- the record has no codec for is a type error that says so, rather than
-- row-types' own about a missing label.
class HasCodec (name :: Symbol) (r :: Row Type) a where
  codecOf :: Label name -> Rec r -> RouteDuplex' a

instance (Lookup (Elem name (Labels r)) name r a) => HasCodec name r a where
  codecOf = lookupCodec (Proxy @(Elem name (Labels r)))

class Lookup (found :: Bool) (name :: Symbol) (r :: Row Type) a where
  lookupCodec :: Proxy found -> Label name -> Rec r -> RouteDuplex' a

instance (KnownSymbol name, HasType name (RouteDuplex' a) r) => Lookup 'True name r a where
  lookupCodec _ name codecs = codecs .! name

instance
  (TypeError ('Text "The constructor " ':<>: 'ShowType name ':<>: 'Text " has no route codec"))
  => Lookup 'False name r a
  where
  lookupCodec _ _ _ = panic "unreachable"

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
