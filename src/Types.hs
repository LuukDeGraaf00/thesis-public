{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import qualified GHC.Show as Prelude
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Elt

import qualified GHC.Num
import Data.Bits
import qualified Data.Array.Accelerate.AST as AST
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Representation.Tag (TagR (TagRsingle), TAG)


-- | generate nullary function that creates a value on each location
nullary :: forall a. (forall t. ScalarType t -> SmartExp t) -> TypeR a -> SmartExp a
nullary f TupRunit       = SmartExp Nil
nullary f (TupRsingle v) = f v
nullary f (TupRpair l r) = SmartExp (Pair (nullary f l) (nullary f r))

-- | generate unary function that applies a function to all values individually
unary :: forall a. (forall t. ScalarType t -> SmartExp t -> SmartExp t) -> SmartExp a -> TypeR a -> SmartExp a
unary f a TupRunit       = a
unary f a (TupRsingle v) = f v a
unary f a (TupRpair l r) = SmartExp (Pair (unary f (SmartExp (Prj PairIdxLeft a)) l) (unary f (SmartExp (Prj PairIdxRight a)) r))

-- | generate binary function that applies a function to all related values individually
binary :: forall a. (forall t. ScalarType t -> SmartExp t -> SmartExp t -> SmartExp t) -> SmartExp a -> SmartExp a -> TypeR a -> SmartExp a
binary f a b TupRunit       = a
binary f a b (TupRsingle v) = f v a b
binary f a b (TupRpair l r) = SmartExp (Pair (binary f (left a) (left b) l) (binary f (right a) (right b) r))
    where left t  = SmartExp (Prj PairIdxLeft t)
          right t = SmartExp (Prj PairIdxRight t)

-- | apply nullary function
nullaryF :: forall e. (Elt e) => (forall t. ScalarType t -> t) -> Exp e
nullaryF f = Exp (nullary (\v -> SmartExp (Const v (f v))) (eltR @e))

-- apply unary function
unaryF :: forall e. (Elt e) => (forall t. ScalarType t -> AST.PrimFun (t -> t)) -> Exp e -> Exp e
unaryF f (Exp e) = Exp (unary (\v a -> SmartExp (PrimApp (f v) a)) e (eltR @e))

-- apply binary function
binaryF :: forall e. (Elt e) => (forall t. ScalarType t -> AST.PrimFun ((t, t) -> t)) -> Exp e -> Exp e -> Exp e
binaryF f (Exp a) (Exp b) = Exp (binary (\v a b -> SmartExp (PrimApp (f v) (SmartExp (Pair a b)))) a b (eltR @e))

-- | paste an integral type on every single field
paste :: forall a b. (Elt b, IsIntegral (EltR a)) => Exp a -> Exp b
paste (Exp e) = Exp (nullary (f e) (eltR @b))
  where f value (SingleScalarType (NumSingleType v)) = SmartExp (PrimApp (AST.PrimFromIntegral integralType v) value)
        f _     (VectorScalarType _)                 = error "did not yet implement vectors!"

-- | generates bool mask from a boolean
mask :: forall a. (Elt a) => Exp a -> Exp a
mask (Exp e) = Exp (unary f e (eltR @a))
  where f (SingleScalarType (NumSingleType (IntegralNumType v))) a = SmartExp (PrimApp (AST.PrimNeg (IntegralNumType v)) a)
        f (SingleScalarType (NumSingleType (FloatingNumType v))) a = SmartExp (PrimApp (AST.PrimNeg undefined) a)
        f _ _                                                      = undefined

-- | generate mask based on boolean
masking :: forall a. (Elt a) => Exp Bool -> Exp a
masking = mask . paste . (mkCoerce :: Exp Bool -> Exp TAG)

-- | unconditional function (works only on integrals for now)
uncond :: forall e. Elt e => Exp Bool -> Exp e -> Exp e -> Exp e
uncond bool left right = binaryF (\(SingleScalarType (NumSingleType (IntegralNumType v))) -> AST.PrimBOr v) (onTrue mask left) (onFalse mask right)
    where mask = masking bool :: Exp e

onTrue :: forall e. (Elt e) => Exp e -> Exp e -> Exp e
onTrue = binaryF (\(SingleScalarType (NumSingleType (IntegralNumType v))) -> AST.PrimBAnd v)

onFalse :: forall e. (Elt e) => Exp e -> Exp e -> Exp e
onFalse (Exp mask) (Exp value) = Exp $ binary (\(SingleScalarType (NumSingleType (IntegralNumType v))) a b -> SmartExp (PrimApp (AST.PrimBAnd v) (SmartExp (Pair (SmartExp (PrimApp (AST.PrimBNot v) a)) b)))) mask value (eltR @e)

example5 :: Exp (Int, Word)
example5 = uncond False_ (T2 3 1) (T2 5 5)



-- | construct a scalar
scalarF :: (SingleType a -> a) -> (VectorType a -> a) -> ScalarType a -> a
scalarF a _ (SingleScalarType r) = a r
scalarF _ b (VectorScalarType r) = b r

-- | construct single
singleF :: (NumType a -> a) -> SingleType a -> a
singleF a (NumSingleType r) = a r

-- | construct numerical
numF :: (IntegralType a -> a) -> (FloatingType a -> a) -> NumType a -> a
numF a _ (IntegralNumType r) = a r
numF _ b (FloatingNumType r) = b r

-- | convert
integralF :: (IntegralDict t -> t) -> IntegralType t -> t
integralF f = f . integralDict

-- | convert
floatingF :: (FloatingDict t -> t) -> FloatingType t -> t
floatingF f = f . floatingDict

-- | zero
zeroB :: IntegralDict t -> t
zeroB IntegralDict = zeroBits

-- | all
allB :: IntegralDict t -> t
allB IntegralDict = complement zeroBits

-- | max
maxI :: IntegralDict t -> t
maxI IntegralDict = maxBound

-- | min
minI :: IntegralDict t -> t
minI IntegralDict = minBound

fl :: FloatingDict t -> t
fl FloatingDict = undefined



-- HELPERS


mat :: ScalarType t -> AST.PrimFun (t -> t)
mat (SingleScalarType (NumSingleType v)) = AST.PrimNeg v
mat _ = error "unexpected"

add :: ScalarType t -> AST.PrimFun ((t, t) -> t)
add (SingleScalarType (NumSingleType v)) = AST.PrimAdd v
add _ = error "unexpected"

and :: ScalarType t -> AST.PrimFun ((t, t) -> t)
and (SingleScalarType (NumSingleType (IntegralNumType v))) = AST.PrimBAnd v
and _ = error "unexpected"

as :: Acc (Array DIM1 (Int, Int))
as = A.map (unaryF mat) list
  where list = use (fromList (Z :. 2) [(4, 0), (-1, 10)])

bs :: Acc (Array DIM1 Element)
bs = A.map (unaryF mat) list
  where list = use (fromList (Z :. 2) [Fire 5, None 1])


example :: Exp (Word, Int)
example = nullaryF (scalarF (singleF (numF (integralF allB) undefined)) undefined)

example2 :: Exp (Int, Int)
example2 = unaryF mat (T2 5 5 :: Exp (Int, Int))

example3 :: Exp (Int, Int)
example3 = binaryF Types.and (T2 5 5 :: Exp (Int, Int)) (T2 3 2)

example4 :: Exp Element
example4 = unaryF mat (constant (Fire 5))

data Element = Oil !Int8 | Fire !Int8 | Water !Int8 | None !Int8
  deriving (Show, Generic)


instance Elt Element where

  type EltR Element = Int8

  eltR :: TypeR (EltR Element)
  eltR = TupRsingle scalarType
  
  tagsR :: [TagR (EltR Element)]
  tagsR = [TagRsingle scalarType]
  
  toElt :: EltR Element -> Element
  toElt n | n Prelude.< 4  = Oil n
          | n Prelude.< 16 = Fire n
          | n Prelude.< 32 = Water n
          | otherwise      = None n
  
  fromElt :: Element -> EltR Element
  fromElt (Oil n)   = n
  fromElt (Fire n)  = n 
  fromElt (Water n) = n
  fromElt (None n)  = n



data V (types :: [*]) where

    Variant :: (Elt e) => Exp e -> V types



test :: V [Int, Int] -> Int
test (Variant x) = undefined