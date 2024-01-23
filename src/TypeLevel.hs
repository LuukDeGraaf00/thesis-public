{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module TypeLevel where
    
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type
import GHC.TypeLits
import GHC.TypeNats
import Data.Array.Accelerate.Representation.Type
import Data.Data

-- | preserving representation that ensures no truncating happens
type Preserving (types :: [r]) = NatTypes (SplitBitSize '[] types)

type Naive (types :: [r]) = NatTypes (NormalBitSize types)

-- | Higher order map function
type family Map (f :: * -> *) xs :: [*] where

    Map f '[]      = '[]
    Map f (x : xs) = f x : Map f xs

-- | Higher order foldr function
type family Foldr (f :: a -> b -> b) (x :: b) (xs :: [a]) :: b where

    Foldr f z '[]       = z
    Foldr f z (x : xs)  = f x (Foldr f z xs)

-- | Higher order foldl function
type family Foldl (f :: a -> b -> b) (x :: b) (xs :: [a]) :: b where

    Foldl f z '[]       = z
    Foldl f z (x : xs)  = Foldl f (f z x) xs

-- | returns True if all items in the list fulfill the condition
type family All (f :: a -> Bool) (xs :: [a]) :: Bool where

    All f '[]           = True
    All f (x : xs)      = If (f x) False (All f xs)

-- | returns true if at least one item in the list fulfills the condition
type family Any (f :: a -> Bool) (xs :: [a]) :: Bool where

    Any f '[]           = False
    Any f (x : xs)      = If (f x) True (Any f xs)

-- | returns true on equality
type family Elem a (b :: [*]) :: Bool where

    Elem x '[]       = False        -- no
    Elem x (x : ys)  = True         -- yes
    Elem x (y : ys)  = Elem x ys    -- no, but recurse

-- | append list
type (xs :: [r]) ++ (ys :: [r]) = Foldr (:) ys xs

-- | append only if unique
type family SetInsert a (b :: [r]) :: [r] where

    SetInsert x (x : ys)  = x : ys              -- no
    SetInsert x (y : '[]) = y : (x : '[])       -- yes
    SetInsert x (y : ys)  = y : SetInsert x ys  -- no, but recurse

-- | remove 
type family RemoveAll a (b :: [r]) :: [r] where

    RemoveAll x '[]       = '[]                   -- no
    RemoveAll x (x : ys)  = RemoveAll x ys        -- yes, and recurse
    RemoveAll x (y : ys)  = y : RemoveAll x ys    -- no, but recurse

type family RemoveOne a (b :: [r]) :: [r] where

    RemoveOne x '[]       ='[]                    -- no
    RemoveOne x (x : ys)  = ys                    -- yes, and do not recurse
    RemoveOne x (y : ys)  = y : RemoveOne x ys    -- no, but recurse

-- | union of two lists 
type family SetUnion (a :: [r]) (b :: [r]) :: [r] where

    SetUnion '[]       ys = ys 
    SetUnion (x : xs)  ys = SetUnion xs (SetInsert x ys)

-- | remove all instances of a within b
type family SetMinus (a :: [r]) (b :: [r]) :: [r] where

    SetMinus '[]       ys  = ys                      
    SetMinus (x : xs)  ys  = SetMinus xs (RemoveAll x ys) 

-- | only if element exists in both list
type family Intersect (a :: [r]) (b :: [r]) :: [r] where

    Intersect '[]       ys  = '[]
    Intersect (x : xs)  ys  = If (Elem x ys) (x : Intersect xs ys) (Intersect xs ys)

-- | empty list
type family Empty (a :: [r]) :: Bool where

    Empty '[] = True
    Empty a   = False

-- | conditional
type family If (conditional :: Bool) a b where

    If True  a b = a
    If False a b = b 


type family NatTypes (as :: [Nat]) :: * where

    NatTypes '[]       = ()
    NatTypes (x : '[]) = NatType x
    NatTypes (x : xs)  = (NatType x, NatTypes xs)
    
-- | translation natural to value
type family NatType (a :: Nat) :: * where

    NatType 0  = ()
    NatType 8  = Word8
    NatType 16 = Word16
    NatType 32 = Word32
    NatType 64 = Word64
    NatType a  = VarNatType (1 + Log2 a)  -- log2 rounds down, so we add one and filter exact matches out before

-- | variable sized type, which is currently rounded up
type family VarNatType (a :: Nat) :: * where

    VarNatType 1 = Word8
    VarNatType 2 = Word8
    VarNatType 3 = Word8
    VarNatType 4 = Word16
    VarNatType 5 = Word32
    VarNatType 6 = Word64
    VarNatType a = TypeError (Text "large variable sizes are not yet supported!") 

-- | size of snoc-like type (if bool = 1 :: change to byte size)
type family MaximumBitSize (a :: Nat) (b :: [r]) :: Nat where

    MaximumBitSize a '[]      = a
    MaximumBitSize a (b : bs) = MaximumBitSize' a (SnocBitSize (EltR b)) bs

-- | recursive instances that avoids recomputation
type MaximumBitSize' (a :: Nat) (b :: Nat) (bs :: [*]) = If (a <=? b) (MaximumBitSize b bs) (MaximumBitSize a bs)


-- | extracts all types from a snoc-like type
type family Fields a :: [*] where

    Fields (a, b)   = Fields a ++ Fields b 
    Fields ()       = '[]                   
    Fields a        = a : '[]  

-- | extracts total bit size from a snoc-like type
type family SnocBitSize a :: Nat where

    SnocBitSize ()       = 0
    SnocBitSize (a, b)   = SnocBitSize a + SnocBitSize b 
    SnocBitSize a        = BitSize a

-- | extracts all types from a snoc-like type
type family FieldsSize a :: [Nat] where

    FieldsSize (a, b)   = FieldsSize a ++ FieldsSize b 
    FieldsSize ()       = '[]                   
    FieldsSize a        = BitSize a : '[]            

-- | 
type family Difference (a :: [r]) (b :: [r]) :: [r] where

    Difference '[]      bs = bs
    Difference (a : as) bs = a : Difference as (RemoveOne a bs)


type family SplitBitSize (a :: [Nat]) (b :: [r]) :: [Nat] where

    SplitBitSize xs  '[]      = xs
    SplitBitSize xs  (y : ys) = SplitBitSize (Difference xs (FieldsSize (EltR y))) ys


type family NormalBitSize (a :: [r]) :: [Nat] where

    NormalBitSize '[]      = '[]
    NormalBitSize (x : xs) = FieldsSize (EltR x) ++ NormalBitSize xs


type family Length (xs :: [r]) :: Nat where

    Length '[]      = 0
    Length (x : xs) = 1 + Length xs


type family IX (index :: Nat) (types :: [t]) :: t where

    IX 0 (x : xs) = x
    IX n (x : xs) = IX (n - 1) xs
    IX n '[]      = TypeError (Text "Index out of range!")

type family Sort (types :: [Nat]) :: [Nat] where
  Sort '[]      = '[]
  Sort (x : xs) = Insert x (Sort xs)

type family Insert x xs where
  Insert x '[]       = x ': '[]
  Insert x (y ': ys) = Insert' (CmpNat x y) x y ys

type family Insert' b x y ys where
  Insert' 'LT  x y ys = x ': (y ': ys)
  Insert' _    x y ys = y ': Insert x ys
