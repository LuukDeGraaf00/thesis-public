{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}


module Implementation where

import Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate as A
import qualified Prelude as P

import TypeLevel
import Variant
import Generics
import Data.Array.Accelerate.Sugar.Elt
import Data.Typeable
import Data.Array.Accelerate

import Data.Array.Accelerate.Data.Sort.Quick ( sortBy )
import qualified GHC.TypeNats

-- | compact sum type representation
type CV (types :: [r]) = Variant (Constructor CRep) types
type NV (types :: [r]) = Variant (Constructor NRep) types

-- | compact representation
type Compact (types :: [r]) = NatTypes (SplitBitSize '[] types)

-- | key used for dictionary
data CRep
data NRep

-- | dictionary instance for application of a compact representation
newtype instance Constructor CRep types = CRep (Compact types)
newtype instance Constructor NRep types = NRep (Compact types)


-- | elt instance
instance (Elt (Compact types)) => Elt (Constructor CRep types) where

    type EltR (Constructor CRep types) = EltR (Compact types)

    eltR = eltR @(Compact types)

    tagsR = tagsR @(Compact types)

    fromElt = undefined

    toElt = undefined


data Collection (types :: [*]) = ElementWise (Acc (Vector (CV types))) | SortedWise (Acc (Vector (CV types))) | VariantWise [VariantOf]


instance (Elt (Compact types), GHC.TypeNats.KnownNat (Length types)) => Show (Collection types) where

    show (SortedWise a)  = P.show a
    show (ElementWise a) = P.show a
    show (VariantWise a) = P.concatMap P.show a

data VariantOf = forall a. (Elt a) => VariantOf (Acc (Vector a))

instance Show VariantOf where
    
    show (VariantOf v) = P.show v


-- | inserts a collection (forces variant-wise collection)
insert :: (Elt v) => Acc (Vector v) -> Collection vs -> Collection (v : vs)
insert v (VariantWise vs) = VariantWise (VariantOf v : vs)
insert _ _                = error "not yet supported: first sort"

-- | extracts a collection (forces sorted-wise collection)
extract :: (Elt (Compact vs), GHC.TypeNats.KnownNat (Length vs)) => Collection vs -> Acc (Vector (CV vs))
extract (SortedWise vs)  = vs
extract (ElementWise vs) = vs
extract (VariantWise vs) = extract (sortedWise (VariantWise vs))

-- | operates on all variants
map :: (Elt (CV vs), Elt r) => (Exp (CV vs) -> Exp r) -> Collection vs -> Acc (Vector r)
map fs (VariantWise vs) = P.foldl1 (A.++) (P.zipWith (\(VariantOf v) f -> A.map (f . Construct) v) vs (functions fs))
map f  (SortedWise  vs) = A.map (match f) vs
map f  (ElementWise vs) = A.map (match f) vs

-- | force variant-wise collection
variantWise :: (Elt (CV vs), Elt (Compact vs)) => Collection vs -> Collection vs
variantWise (ElementWise vs) = variantWise (SortedWise (sortBy (\a b -> compare (unsafeToTag a) (unsafeToTag b)) vs))
variantWise (SortedWise vs)  = undefined --VariantWise (P.map (\(n :: Word8) -> VariantOf (A.afst $ A.filter (\a -> unsafeToTag a A.== constant n) vs)) [0..])
variantWise (VariantWise vs) = VariantWise vs

-- | force sorted-wise collection
sortedWise :: (Elt (CV vs), Elt (Compact vs)) => Collection vs -> Collection vs
sortedWise (VariantWise vs) = SortedWise (P.foldl1 (A.++) (P.zipWith (\(VariantOf v) n -> A.map (setTagN n) v) vs [0..]))
sortedWise (ElementWise vs) = SortedWise (sortBy (\a b -> compare (unsafeToTag a) (unsafeToTag b)) vs)
sortedWise (SortedWise  vs) = SortedWise vs

 -- store as function that can transform to CV version
mempty :: Collection '[]
mempty = VariantWise []

apply :: (Elt (Compact vs), GHC.TypeNats.KnownNat (Length vs)) => Collection vs -> Vector (CV vs)
apply = I.run . extract

values = insert ints (insert floats mempty)

result = Implementation.map (\case
    {
        Con0 f -> f;
        Con1 f -> round (f * 5);
        _      -> 0
    }) values


floats :: Acc (Vector Float)
floats = use (A.fromList (Z :. 100) [1..100])

ints :: Acc (Vector Int32)
ints = use (A.fromList (Z :. 100) [1..100])


printRep :: forall a. (Elt a) => Exp a -> P.String
printRep _ = P.show (eltR @a)

daf = P.show $ eltR @(CV [Float, Float, Maybe Float])
dah = P.show $ tagsR @(CV [Float, Int, Maybe Float])