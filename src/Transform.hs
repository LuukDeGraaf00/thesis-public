{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Transform where

import qualified Prelude as P
import qualified Data.Foldable as F
import qualified Data.Array.Accelerate.AST as AST

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt

import Types

import Data.Type.Equality ( type (:~:)(..) )

-- | get value from expression
type Get value expression = SmartExp expression -> SmartExp value

-- | set value within an existing expression
type Set value expression = SmartExp value -> SmartExp expression -> SmartExp expression

-- | resulting function that generates an isomorphic adjustment
type Result a b = (SmartExp b -> SmartExp a -> SmartExp b, SmartExp a -> SmartExp b -> SmartExp a)

-- | establish a mapping between two fields
type Mapping a b = Field a -> Field b -> Maybe (Result a b)

-- | tuple of fields
type Fields a b = ([Field a], [Field b])

-- | getter and setter for a field within a datatype
data Field  e = forall v. Field !(ScalarType v) !(Set v e) !(Get v e)

-- | retrieve value from expression
data Getter e = forall v. Getter !(ScalarType v) !(Get v e)

-- | insert value into expression
data Setter e = forall v. Setter !(ScalarType v) !(Set v e)

-- | inserts a datatype into another datatype 
insert :: forall v t. (Elt v, Elt t) => (Exp v -> Exp t, Exp t -> Exp v) 
insert = (\(Exp v) -> Exp (l (unExp (undef :: Exp t)) v), \(Exp t) -> Exp (r (unExp (undef :: Exp v)) t))
  where (l, r) = information @v @t [equal, equalSize, largerSize]

-- | extracts a datatype from another datatype
extract :: forall t v. (Elt t, Elt v) => (Exp t -> Exp v, Exp v -> Exp t)
extract = let (l, r) = insert in (r, l)

-- | transforms all datatypes, no data loss within fields but no established 'subset' relation 
transformSafe:: forall v t. (Elt v, Elt t) => (Exp v -> Exp t, Exp t -> Exp v)
transformSafe = (\(Exp v) -> Exp (l (unExp (undef :: Exp t)) v), \(Exp t) -> Exp (r (unExp (undef :: Exp v)) t))
  where (l, r) = structure @v @t [equal, equalSize]

-- | transforms all datatypes, but data loss between transformation can exist
transformUnsafe :: forall v t. (Elt v, Elt t) => (Exp v -> Exp t, Exp t -> Exp v)
transformUnsafe = (\(Exp v) -> Exp (l (unExp (undef :: Exp t)) v), \(Exp t) -> Exp (r (unExp (undef :: Exp v)) t))
  where (l, r) = structure @v @t [equal, equalSize, coerceUnsafe]

-- | extracts all fields of a datatype
fields :: forall e. TypeR e -> [Field e]
fields = traverse (\s insert retrieve -> [Field s insert retrieve])

-- | setter for a field of a datatype
setters :: forall e. TypeR e -> [Setter e]
setters = traverse (\s insert retrieve -> [Setter s insert])

-- | getter for a field of a datatype
getters :: forall e. TypeR e -> [Getter e]
getters = traverse (\s insert retrieve -> [Getter s retrieve])

-- | structure preserving isomorphic mapping between two datatypes
structure :: forall a b. (Elt a, Elt b) => [Mapping (EltR a) (EltR b)] -> Result (EltR a) (EltR b)
structure fs = case isomorphism (eltR @a) (eltR @b) fs of
    (xs, (as@(_:_), bs@(_:_))) -> error ("Could not establish an inverse mapping between " P.++ P.show as P.++ " and " P.++ P.show bs)
    (xs, _)                    -> xs

-- | information preserving mapping which enforces that the fields of the argument are used within the result
information :: forall a b. (Elt a, Elt b) => [Mapping (EltR a) (EltR b)] -> Result (EltR a) (EltR b)
information fs = case isomorphism (eltR @a) (eltR @b) fs of
    (xs, (as@(_:_), bs)) -> error ("Could not insert " P.++ P.show as P.++ " into remaining " P.++ P.show bs)
    (xs, _)              -> xs

-- | creates an isomorphic mapping between two datatypes
isomorphism :: forall a b. TypeR a -> TypeR b -> [Mapping a b] -> (Result a b, Fields a b)
isomorphism a b = F.foldl' (\(result, xs) f -> mapping f xs (result, ([], []))) ((P.const, P.const), (fields a, fields b))
  where 

    -- | recursively maps fields
    mapping :: Mapping a b -> Fields a b -> (Result a b, Fields a b) -> (Result a b, Fields a b)
    mapping f (zs, bs) ((x, y), (xs, ys)) = case (zs, bs) of
       ([],bs)      -> ((x, y), (xs, ys))
       (as,[])      -> ((x, y), (as P.++ xs, [])) -- append with final 
       (a : as, _)  -> case single f a bs of
              (rs, Just (x2, y2))  -> mapping f (as, rs) ((\b a -> x (x2 b a) a, \a b -> y (y2 a b) b), (xs, rs))
              (rs, Nothing)        -> mapping f (as, rs) ((x, y), (a : xs, rs))

    -- | attempts to map a single field
    single :: Mapping a b -> Field a -> [Field b] -> ([Field b], Maybe (Result a b))
    single f a = F.foldl' (\(bs, r) b -> P.maybe (P.maybe (b : bs, Nothing) (\v -> (bs, Just v)) (f a b)) (\v -> (b : bs, Just v)) r)  ([], Nothing)

-- | equality
equal :: forall a b. Mapping a b
equal (Field a l r) (Field b x y) = P.fmap (\Refl -> (\b a -> x (r a) b, \a b -> l (y b) a)) (equalScalar a b)

-- | equal size
equalSize :: forall a b. Mapping a b
equalSize l@(Field a _ _) r@(Field b _ _) = if sizeScalar a P.== sizeScalar b then coerceUnsafe l r else Nothing

-- | larger size (loss of data only for returning function)
largerSize :: forall a b. Mapping a b
largerSize l@(Field a _ _) r@(Field b _ _) = if sizeScalar a P.<= sizeScalar b then coerceUnsafe l r else Nothing

-- | unsafe coercion between all scalar datatypes
coerceUnsafe :: forall a b. Mapping a b
coerceUnsafe (Field (SingleScalarType (NumSingleType a)) setA getA) (Field (SingleScalarType (NumSingleType b)) setB getB) = case (a, b) of 
          (IntegralNumType a1, IntegralNumType b1) -> Just (fromInt   a1 getA b  setB, fromInt   b1 getB a  setA)
          (IntegralNumType a1, FloatingNumType b1) -> Just (fromInt   a1 getA b  setB, fromFloat b1 getB a1 setA)
          (FloatingNumType a1, IntegralNumType b1) -> Just (fromFloat a1 getA b1 setB, toFloat   b  getB a1 setA)
          (FloatingNumType a1, FloatingNumType b1) -> Just (toFloat   a  getA b1 setB, toFloat   b  getB a1 setA)
coerceUnsafe _ _ = Nothing
    
-- helper functions

fromInt :: IntegralType v -> Get v a -> NumType r -> Set r b -> SmartExp b -> SmartExp a -> SmartExp b
fromInt v get r set b a = set (SmartExp (PrimApp (AST.PrimFromIntegral v r) (get a))) b

toFloat :: NumType v -> Get v a -> FloatingType r -> Set r b -> SmartExp b -> SmartExp a -> SmartExp b
toFloat v get r set b a = set (SmartExp (PrimApp (AST.PrimToFloating v r) (get a))) b

fromFloat :: FloatingType v -> Get v a -> IntegralType r -> Set r b -> SmartExp b -> SmartExp a -> SmartExp b
fromFloat TypeHalf   get r set b a = set (SmartExp (PrimApp (AST.PrimFromIntegral TypeWord16 (IntegralNumType r)) (SmartExp (Coerce TF16 TW16 (get a))))) b
fromFloat TypeFloat  get r set b a = set (SmartExp (PrimApp (AST.PrimFromIntegral TypeWord32 (IntegralNumType r)) (SmartExp (Coerce TF32 TW32 (get a))))) b
fromFloat TypeDouble get r set b a = set (SmartExp (PrimApp (AST.PrimFromIntegral TypeWord64 (IntegralNumType r)) (SmartExp (Coerce TF64 TW64 (get a))))) b

sizeScalar :: ScalarType t -> Int
sizeScalar (SingleScalarType t) = sizeSingle t
sizeScalar (VectorScalarType t) = sizeVector t

sizeVector :: VectorType t -> Int
sizeVector (VectorType n t) = n * sizeSingle t

sizeSingle :: SingleType t -> Int
sizeSingle (NumSingleType t) = sizeNum t

sizeNum :: NumType t -> Int
sizeNum (IntegralNumType t) = sizeIntegral t
sizeNum (FloatingNumType t) = sizeFloating t

sizeIntegral :: IntegralType t -> Int
sizeIntegral TypeInt    = 8
sizeIntegral TypeInt8   = 1
sizeIntegral TypeInt16  = 2
sizeIntegral TypeInt32  = 4
sizeIntegral TypeInt64  = 8
sizeIntegral TypeWord   = 8
sizeIntegral TypeWord8  = 1
sizeIntegral TypeWord16 = 2
sizeIntegral TypeWord32 = 4
sizeIntegral TypeWord64 = 8

sizeFloating :: FloatingType t -> Int
sizeFloating TypeHalf   = 2
sizeFloating TypeFloat  = 4
sizeFloating TypeDouble = 8

equalScalar :: forall a b. ScalarType a -> ScalarType b -> Maybe (a :~: b)
equalScalar TI   TI   = Just Refl
equalScalar TI8  TI8  = Just Refl
equalScalar TI16 TI16 = Just Refl
equalScalar TI32 TI32 = Just Refl
equalScalar TI64 TI64 = Just Refl
equalScalar TW   TW   = Just Refl
equalScalar TW8  TW8  = Just Refl
equalScalar TW16 TW16 = Just Refl
equalScalar TW32 TW32 = Just Refl
equalScalar TW64 TW64 = Just Refl
equalScalar TF16 TF16 = Just Refl
equalScalar TF32 TF32 = Just Refl
equalScalar TF64 TF64 = Just Refl
equalScalar _ _       = Nothing

instance Show (Field e) where

  show (Field a f g) = P.show a

instance Show (Setter e) where
  show (Setter a f) = P.show a

instance Show (Getter e) where
  show (Getter a f) = P.show a