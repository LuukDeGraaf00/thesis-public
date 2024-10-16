{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Variant where

import qualified Prelude as P

import qualified Data.Map

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import Data.Typeable
import Transform
import Generics
import TypeLevel
import qualified GHC.TypeLits as NAT
import Data.Dynamic

-- | useful variant type that constructs the representation
type V value (variants :: [variant]) = Variant (Constructor value) variants

-- | generic variant type
data Variant (function :: [variant] -> *) (variants :: [variant]) = VarUnion TAG (EltR (function variants))

-- | constructor data family that creates a representation constructor
data family Constructor argument :: [variant] -> *

-- | generic sum type implementation
instance (Elt (compact types), NAT.KnownNat (Length types)) => Elt (Variant compact types) where

    -- | tag with a compact representation
    type EltR (Variant compact types) = (TAG, EltR (compact types))

    -- | borrow representation from the assumption we return a valid type
    eltR :: TypeR (EltR (Variant compact types))
    eltR = TupRpair (TupRsingle TW8) (eltR @(compact types))
    
    -- | linear creation of tags (no nested pattern matching)
    tagsR :: [TagR (EltR (Variant compact types))]
    tagsR = [TagRtag n t | n <- [0..typeListLength @types - 1], t <- (tagsR @(compact types))]
    
    -- | raw data
    toElt :: EltR (Variant compact types) -> Variant compact types
    toElt (t, v) = VarUnion t v
    
    -- | raw data
    fromElt :: Variant compact types -> EltR (Variant compact types)
    fromElt (VarUnion t v) = (t, v)

-- | default relation between variants
class (Elt t, Elt v) => Element t v where

  -- | insert variant
  construct :: Exp v -> Exp t
  construct = Construct

  -- | extract variant
  destruct :: Exp t -> Exp v
  destruct = Destruct


instance Show (EltR (f vs)) => Show (Variant f vs) where

  show :: Variant f vs -> P.String
  show (VarUnion t v) = P.show t P.++ ":" P.++ P.show v

-- | generate pattern synonym for a variant
pattern Variant :: forall t v. (Element t v) => Exp v -> Exp t
pattern Variant t <- (matching -> (True, t))
  where Variant v = construct @t @v v

-- | generate pattern synonym for an untagged union
pattern Union :: forall t v. (Element t v) => Exp v -> Exp t
pattern Union t <- (destruct @t @v -> t)
  where Union v = construct @t @v v

-- | generate pattern synonym for any transform
pattern Construct :: forall t v. (Elt t, Elt v) => Exp v -> Exp t
pattern Construct value <- (P.snd (insert @v @t) -> value)
  where Construct value = P.fst (insert @v @t) value

-- | generate pattern synonym for any transform
pattern Destruct :: forall t v. (Elt t, Elt v) => Exp v -> Exp t
pattern Destruct value <- (P.fst (insert @t @v) -> value)
  where Destruct value = P.snd (insert @t @v) value

-- | generic constructor pattern synonym
pattern Con :: forall n v vs f. (Elt (IX n vs), Elt (f vs), Typeable (EltR (IX n vs)), NAT.KnownNat n, NAT.KnownNat (Length vs)) => Exp (IX n vs) -> Exp (Variant f vs)
pattern Con v <- (matchable (toWord @n) -> Just v)
  where Con v = constructable (toWord @n) (Construct v :: Exp (f vs))

-- | generic constructor pattern synonym for first element
pattern Con0 :: forall v vs f. (Elt (IX 0 vs), Elt (f vs), Typeable (EltR (IX 0 vs)), NAT.KnownNat (Length vs)) => Exp (IX 0 vs) -> Exp (Variant f vs)
pattern Con0 v <- (matchable 0 -> Just v)
  where Con0 v = constructable 0 (Construct v :: Exp (f vs))

-- | generic constructor pattern synonym for second element
pattern Con1 :: forall v vs f. (Elt (IX 1 vs), Elt (f vs), Typeable (EltR (IX 1 vs)), NAT.KnownNat (Length vs)) => Exp (IX 1 vs) -> Exp (Variant f vs)
pattern Con1 v <- (matchable 1 -> Just v)
  where Con1 v = constructable 1 (Construct v :: Exp (f vs))

-- | generic constructor pattern synonym for third element
pattern Con2 :: forall v vs f. (Elt (IX 2 vs), Elt (f vs), Typeable (EltR (IX 2 vs)), NAT.KnownNat (Length vs)) => Exp (IX 2 vs) -> Exp (Variant f vs)
pattern Con2 v <- (matchable 2 -> Just v)
  where Con2 v = constructable 2 (Construct v :: Exp (f vs))

-- | constructs a variant with the word
constructable :: (EltR t ~ (Word8, EltR v)) => Word8 -> Exp v -> Exp t
constructable word e = Exp (SmartExp (Pair (SmartExp (Const TW8 word)) (unExp e)))

-- | use trace to identity if it matches constructor and the corresponding tag
matchable :: forall v vs f. (Elt v, Typeable (EltR v), Elt (Variant f vs)) => Word8 -> Exp (Variant f vs) -> Maybe (Exp v)
matchable constructor (Exp (SmartExp (Match trace e))) = if constructor P.== getTag trace 
          then Just (Exp (SmartExp (Match (tagsR @v P.!! 0) (unExp (Destruct (Exp e :: Exp (Variant f vs)) :: Exp v)))))
          else Nothing
matchable _ _                                           = Nothing

typeListLength :: forall t r. (NAT.KnownNat (Length t), P.Num r) => r
typeListLength = P.fromIntegral (NAT.natVal (Proxy @(Length t)))

setTagN :: forall a f vs. (Elt a, Elt (f vs)) => Word8 -> Exp a -> Exp (Variant f vs)
setTagN n v = constructable n (Construct v :: Exp (f vs))

getTag :: TagR a -> TAG
getTag (TagRtag w _) =  w
getTag _ = error "oops"

data TagE = forall a. TagE !Word8 !Dynamic


{-

instance Show TagE where

  show (TagE w _) = P.show w

class (Elt a) => Tags a where

  tags :: Word8 -> [TagE]
  tags n = []

instance (Elt v, Typeable (TagR (EltR v)), Elt (V (v : vs)), Tags (V vs)) => Tags (V (v : vs)) where

  tags :: Word8 -> [TagE]
  tags n = P.map (TagE n . toDyn) (tagsR @v) P.++ tags @(V vs) (n + 1)

instance Tags (V '[]) where


type a || b = V [a, b]

dah = tagsR @(V [Maybe Float, Int32])

duh = tags @(V [Maybe Float, Int32]) 0



test :: Exp (Maybe Float || Int32) -> Exp (Int32 || Float)
test (Con0 Nothing_)  = Con0 5
test (Con0 (Just_ f)) = Con1 f
test (Con1 i)         = Con0 i
test _                = Con0 0

-}

{-


-}



-- | expects the match term generated by the 'match' function 
onValue :: forall t v. (Elt t, Elt v) => Word8 -> Exp t -> Maybe (Exp v)
onValue l value@(Exp (SmartExp (Match r t))) | isTag l r = Just (Destruct value :: Exp v) -- add nested later with map (?)
                                             | otherwise = Nothing
onValue l _                            = Nothing


toWord :: forall n. (NAT.KnownNat n) => Word8
toWord = P.fromIntegral (NAT.natVal (Proxy @n))

isTag :: Word8 -> TagR a -> Bool
isTag l TagRunit       = False
isTag l (TagRsingle _) = False
isTag l (TagRundef  _) = False
isTag l (TagRpair _ _) = False
isTag l (TagRtag r _)  = l P.== r


-- | identifier of a particular variant
data Identifier t = forall v. (Element t v) => V (TagR (EltR t))

instance Show (Identifier t) where
  show (V a) = P.show a

-- | abstract data type (maybe insert into elt class (?))
class (Elt t, Typeable t) => Sum t where

  -- | a fixed list of all concrete variants
  variants :: [TypeRep]
  variants = [typeRep (Proxy @t)]

-- | concrete data type
class (Sum t) => Storage t where

  -- | construct tag
  lhs :: Exp t -> Exp TAG

  -- | label
  rhs :: Identifier t -> TAG



-- | expects the match term generated by the 'match' function 
matching :: forall t v. (Element t v) => Exp t -> (Bool, Exp v)
matching (Exp (SmartExp (Match tag e))) = undefined --(create @t @v P.== tag, destruct @t @v (Exp e))
matching _                              = error "Forgot the match!"


pattern Count :: Exp Int -> Exp Int
pattern Count x <- (undefined -> x)

setTag :: forall a. Elt a => Word8 -> TagR (EltR a)
setTag word = tag (eltR @a)
  where tag :: TypeR b -> TagR b
        tag TupRunit                      = TagRunit
        tag (TupRsingle v)                = TagRundef v
        tag (TupRpair (TupRsingle TW8) r) = TagRtag word (tag r)
        tag (TupRpair l r)                = TagRpair     (tag l) (tag r)

succTag :: TagR a -> TagR a
succTag TagRunit       = TagRunit
succTag (TagRsingle v) = TagRsingle v
succTag (TagRundef v)  = TagRundef v
succTag (TagRtag v a)  = TagRtag (v + 1) (succTag a)
succTag (TagRpair a b) = TagRpair (succTag a) (succTag b)

instance P.Eq (TagR a) where

  (==) :: TagR a -> TagR a -> Bool
  (==) TagRunit TagRunit = True
  (==) (TagRsingle _) (TagRsingle _) = True
  (==) (TagRundef  _) (TagRundef  _) = True
  (==) (TagRtag  l a) (TagRtag  r b) = l P.== r P.&& a P.== b
  (==) (TagRpair l a) (TagRpair r b) = l P.== r P.&& a P.== b
  (==)  _ _                          = False

{-

functionsD :: forall a b. (Elt a, Elt b, Sum a) => (Exp a -> Exp b) -> [Exp a -> Exp b]
functionsD f = P.map (\variant@(V context) (Exp input) -> f (Exp $ SmartExp $ Match context input)) (variants @a)

instance (Elt a, Storage a) => Matching (Exp a) where

  type ResultT (Exp a) = a

  mkFun :: (Args f -> Exp (ResultT (Exp a))) -> (Args (Exp a) -> Args f) -> Exp a
  mkFun f k = f (k Result)

  mkMatch :: Exp a -> Args (Exp a) -> Exp (ResultT (Exp a))
  mkMatch (Exp (SmartExp (Match _ x))) Result = Exp x
  mkMatch (Exp e)                      Result = Exp e

instance (Elt e, Storage e, Matching r) => Matching (Exp e -> r) where

  type ResultT (Exp e -> r) = ResultT r

  mkFun :: (Args f -> Exp (ResultT (Exp e -> r))) -> (Args (Exp e -> r) -> Args f) -> Exp e -> r
  mkFun f k x = mkFun f (\xs -> k (x :-> xs))

  mkMatch :: (Exp e -> r) -> Args (Exp e -> r) -> Exp (ResultT (Exp e -> r))
  mkMatch f (x@(Exp (SmartExp Match {})) :-> xs) = mkMatch (f x) xs
  mkMatch f (x@(Exp p)                   :-> xs) = case labels of
             [(_,r)] -> Exp r
             _       -> Exp (SmartExp (Case matchable labels))
    where matchable = SmartExp (Pair (unExp (lhs x)) (SmartExp Nil))
          labels    = P.map (\variant@(V tag) -> (TagRtag ((rhs @e) variant) TagRunit, unExp (mkMatch (f (Exp (SmartExp (Match tag p)))) xs))) (variants @e)


match2 :: Matching f => f -> f
match2 f = mkFun (mkMatch f) P.id


data Args f where
  (:->)  :: Exp a -> Args b -> Args (Exp a -> b)
  Result :: Args (Exp a)

class Matching a where
  type ResultT a
  mkMatch :: a -> Args a -> Exp (ResultT a)
  mkFun   :: (Args f -> Exp (ResultT a))
          -> (Args a -> Args f)
          -> a
-}

-- | generates all resulting functions from all possible patterns.
functions :: forall a b. (Elt a, Elt b) => (Exp a -> Exp b) -> [Exp a -> Exp b]
functions f = P.map (\tag (Exp input) -> f (Exp $ SmartExp $ Match tag input)) (tagsR @a)

-- | generates all resulting functions from all possible patterns, with their corresponding tag.
functionsT :: forall a b. (Elt a, Elt b) => (Exp a -> Exp b) -> [(TagR (EltR a), Exp a -> Exp b)]
functionsT f = P.map (\tag -> (tag, \(Exp input) -> f (Exp $ SmartExp $ Match tag input))) (tagsR @a)

-- | general switch statement
switch :: forall a. (Elt a) => [(TAG, Exp a)] -> (Exp TAG -> Exp a)
switch xs tag     = Exp (SmartExp (Case matchable labels))
  where matchable = SmartExp (Pair (unExp tag) (SmartExp Nil))
        labels    = P.map (\(t, r) -> (TagRtag t TagRunit, unExp r)) xs

-- | generic switch expression
switchT :: forall a b. (Elt a, Elt b) => (Exp a -> Exp TAG) -> [(TAG, Exp a -> Exp b)] -> (Exp a -> Exp b)
switchT tag xs input = Exp (SmartExp (Case matchable labels))
  where matchable    = SmartExp (Pair (unExp (tag input)) (SmartExp Nil))
        labels       = P.map (\(t, f) -> (TagRtag t TagRunit, unExp (f input))) xs


matchJust :: Exp (Maybe a) -> Maybe (Exp a)
matchJust x = case x of
  (Exp (SmartExp (Match (TagRtag 1 (TagRpair TagRunit s)) a))) -> Just (Exp (SmartExp (Match s (SmartExp (Prj PairIdxRight (SmartExp (Prj PairIdxRight a)))))))
  (Exp (SmartExp (Match _ _)))                                 -> Nothing
  _                                                            -> undefined


-- | generate pattern synonym for a variant
pattern Debug :: forall t v. (TestVariant t v) => Exp v -> Exp t
pattern Debug t <- (matching2 -> Just t)
  where Debug v = create @t @v v

-- | expects the match term generated by the 'match' function 
matching2 :: forall t v. (TestVariant t v) => Exp t -> Maybe (Exp v)
matching2 (Exp (SmartExp (Match r e))) = undefined --Just (Exp (SmartExp (Match r e)))
matching2 _                            = Nothing

class (Elt t, Elt v) => TestVariant t v where

  -- | insert variant
  create :: Exp v -> Exp t
  create = Construct

  -- | extract variant
  destroy :: Exp t -> Exp v
  destroy = Destruct