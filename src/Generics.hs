{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}

module Generics where

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
import Data.Array.Accelerate.Representation.Tag


-- helper expression
type Insert value expression   = SmartExp value -> SmartExp expression -> SmartExp expression
type Retrieve value expression = SmartExp expression -> SmartExp value 

-- | traverse expression
traverse :: Monoid r => forall e. (forall a. ScalarType a -> Insert a e -> Retrieve a e -> r) -> TypeR e -> r
traverse f t = loop t f Prelude.const Prelude.id
  where
    loop :: Monoid r => TypeR a -> (forall a. ScalarType a -> Insert a e -> Retrieve a e -> r) -> Insert a e -> Retrieve a e -> r
    loop TupRunit       f insert retrieve = mempty
    loop (TupRsingle v) f insert retrieve = f v insert retrieve
    loop (TupRpair l r) f insert retrieve = loop l f (\a e -> insert (SmartExp (Pair a (SmartExp (Prj PairIdxRight (retrieve e))))) e) (SmartExp . Prj PairIdxLeft  . retrieve) 
                                  `mappend` loop r f (\a e -> insert (SmartExp (Pair (SmartExp (Prj PairIdxLeft (retrieve e))) a)) e)  (SmartExp . Prj PairIdxRight . retrieve)

-- | traverse over representation
traverseRep :: Monoid a => (forall t. ScalarType t -> a) -> TypeR e -> a
traverseRep f TupRunit       = mempty
traverseRep f (TupRsingle v) = f v
traverseRep f (TupRpair l r) = traverseRep f l `mappend` traverseRep f r

-- | traverse over expression and representation
traverseExp :: (Monoid r) => (forall a. ScalarType a -> SmartExp a -> r) -> SmartExp e -> TypeR e -> r
traverseExp f a TupRunit       = mempty
traverseExp f a (TupRsingle v) = f v a
traverseExp f a (TupRpair l r) = traverseExp f (SmartExp (Prj PairIdxLeft a)) l `mappend` traverseExp f (SmartExp (Prj PairIdxRight a)) r
 
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

-- | undefined tag
tagUndef :: TypeR a -> TagR a
tagUndef TupRunit       = TagRunit
tagUndef (TupRsingle v) = TagRundef v
tagUndef (TupRpair l r) = TagRpair (tagUndef l) (tagUndef r)

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

-- | retrieves tag
unsafeToTag :: forall e. (Elt e) => Exp e -> Exp TAG
unsafeToTag (Exp e) = go (eltR @e) e
  where go :: TypeR (EltR e) -> SmartExp (EltR e) -> Exp TAG
        go (TupRsingle TW8) a = Exp a
        go _ _                = error "no tag!"
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



pattern TI :: () => (a ~ Int) => ScalarType a
pattern TI <- SingleScalarType (NumSingleType (IntegralNumType TypeInt))
  where TI = SingleScalarType (NumSingleType (IntegralNumType TypeInt))

pattern TI8 :: () => (a ~ Int8) => ScalarType a
pattern TI8 <- SingleScalarType (NumSingleType (IntegralNumType TypeInt8))
  where TI8 = SingleScalarType (NumSingleType (IntegralNumType TypeInt8))

pattern TI16 :: () => (a ~ Int16) => ScalarType a
pattern TI16 <- SingleScalarType (NumSingleType (IntegralNumType TypeInt16))
  where TI16 = SingleScalarType (NumSingleType (IntegralNumType TypeInt16))

pattern TI32 :: () => (a ~ Int32) => ScalarType a
pattern TI32 <- SingleScalarType (NumSingleType (IntegralNumType TypeInt32))
  where TI32 = SingleScalarType (NumSingleType (IntegralNumType TypeInt32))

pattern TI64 :: () => (a ~ Int64) => ScalarType a
pattern TI64 <- SingleScalarType (NumSingleType (IntegralNumType TypeInt64))
  where TI64 = SingleScalarType (NumSingleType (IntegralNumType TypeInt64))

pattern TW :: () => (a ~ Word) => ScalarType a
pattern TW <- SingleScalarType (NumSingleType (IntegralNumType TypeWord))
  where TW = SingleScalarType (NumSingleType (IntegralNumType TypeWord))

pattern TW8 :: () => (a ~ Word8) => ScalarType a
pattern TW8 <- SingleScalarType (NumSingleType (IntegralNumType TypeWord8))
  where TW8 = SingleScalarType (NumSingleType (IntegralNumType TypeWord8))

pattern TW16 :: () => (a ~ Word16) => ScalarType a
pattern TW16 <- SingleScalarType (NumSingleType (IntegralNumType TypeWord16))
  where TW16 = SingleScalarType (NumSingleType (IntegralNumType TypeWord16))

pattern TW32 :: () => (a ~ Word32) => ScalarType a
pattern TW32 <- SingleScalarType (NumSingleType (IntegralNumType TypeWord32))
  where TW32 = SingleScalarType (NumSingleType (IntegralNumType TypeWord32))

pattern TW64 :: () => (a ~ Word64) => ScalarType a
pattern TW64 <- SingleScalarType (NumSingleType (IntegralNumType TypeWord64))
  where TW64 = SingleScalarType (NumSingleType (IntegralNumType TypeWord64))

pattern TF16 :: () => (a ~ Half) => ScalarType a
pattern TF16 <- SingleScalarType (NumSingleType (FloatingNumType TypeHalf))
  where TF16 = SingleScalarType (NumSingleType (FloatingNumType TypeHalf))

pattern TF32 :: () => (a ~ Float) => ScalarType a
pattern TF32 <- SingleScalarType (NumSingleType (FloatingNumType TypeFloat))
  where TF32 = SingleScalarType (NumSingleType (FloatingNumType TypeFloat))

pattern TF64 :: () => (a ~ Double) => ScalarType a
pattern TF64 <- SingleScalarType (NumSingleType (FloatingNumType TypeDouble))
  where TF64 = SingleScalarType (NumSingleType (FloatingNumType TypeDouble))
