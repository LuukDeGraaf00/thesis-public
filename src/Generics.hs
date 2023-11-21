{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Generics where

import qualified Prelude
import GHC.Generics
import Data.Bits
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Type

import Data.Kind
import Data.Array.Accelerate.Smart


-- | structural sum type
data V (types :: [x]) 
    deriving (Generic)




--class Compact a where

--  type CompactR a :: Data.Kind.Type
--  type CompactR a = GCompactR () (Rep (Int, Int))
    

-- | variant of a particular datatype (subset)
class (Elt t, Elt v) => Variant t v where

  type VariantR t v

instance (Elt t, Elt v) => Variant t v where

  type VariantR t v = GSumR (Rep (EltR t)) (Rep (EltR v))


-- | compact representation of variant
class GSum v t where

    -- | compact result type
    type GSumR v t

    -- | inserts variant into type
    insert :: SmartExp (v a) -> SmartExp (GSumR v t)

    -- | extract variant from type
    extract :: SmartExp (GSumR v t) -> SmartExp (v a)


instance GSum U1 U1 where

    type GSumR U1 U1 = ()

    insert _ = SmartExp Nil

    extract _ = undefined

instance GSum (l :*: r) a where

    type GSumR (l :*: r) a = ()

    insert _ = SmartExp Nil

    extract _ = undefined

{-
instance GCompact a => GCompact (M1 i c a) where

    type GCompactR t (M1 i c a) = GCompactR t a

instance Compact a => GCompact (K1 i a) where

    type GCompactR t (K1 i a) = (CompactR a)

instance (GCompact a, GCompact b) => GCompact (a :*: b) where
    
    type GCompactR t (a :*: b) = GCompactR (GCompactR t a) b

instance (GCompact a, GCompact b) => GCompact (a :+: b) where

    type GCompactR t (a :+: b) = (CompactR a)

-}


class Serialize a where
  put :: a -> [Bit]

  default put :: (Generic a, GSerialize (Rep a)) => a -> [Bit]
  put a = gput (from a)

  get :: [Bit] -> (a, [Bit])

  default get :: (Generic a, GSerialize (Rep a)) => [Bit] -> (a, [Bit])
  get xs = (to x, xs')
    where (x, xs') = gget xs

class GSerialize f where
  gput :: f a -> [Bit]
  gget :: [Bit] -> (f a, [Bit])

-- | Unit: used for constructors without arguments
instance GSerialize U1 where
  gput U1 = []
  gget xs = (U1, xs)

-- | Products: encode multiple arguments to constructors
instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
  gput (a :*: b) = gput a Prelude.++ gput b
  gget xs = (a :*: b, xs'')
    where (a, xs') = gget xs
          (b, xs'') = gget xs'

-- | Sums: encode choice between constructors
instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
  gput (L1 x) = O : gput x
  gput (R1 x) = I : gput x
  gget (O:xs) = (L1 x, xs')
    where (x, xs') = gget xs
  gget (I:xs) = (R1 x, xs')
    where (x, xs') = gget xs

-- | Meta-information (constructor names, etc.)
instance (GSerialize a) => GSerialize (M1 i c a) where
  gput (M1 x) = gput x
  gget xs = (M1 x, xs')
    where (x, xs') = gget xs

-- | Constants, additional parameters and recursion of kind *
instance (Serialize a) => GSerialize (K1 i a) where
  gput (K1 x) = put x
  gget xs = (K1 x, xs')
    where (x, xs') = get xs

instance Serialize Bool where
  put True = [I]
  put False = [O]
  get (I:xs) = (True, xs)
  get (O:xs) = (False, xs)

data Bit = O | I deriving Show

--
-- Try it out. (Normally this would be in a separate module.)
--

data UserTree a = Node a (UserTree a) (UserTree a) | Leaf
  deriving (Generic, Show)

instance (Serialize a) => Serialize (UserTree a)
