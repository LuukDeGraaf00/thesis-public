{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Matching where

import qualified Prelude

import qualified Data.Map

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Data.Bits
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import GHC.Generics

import Data.Typeable


-- | variant type
type Variant = Int

-- | abstract enumeration of all possible variations of a particular datatype
class (Elt a) => Variants a where

  -- | enumerates all variants of a datatype
  variants :: [Variant]

  -- | temporary solution to be able match the tag
  temp :: Variant -> TagR (EltR a)

-- | concrete storage and extraction of a variant 
class (Variants a) => Storage a where

  -- | runtime extraction of a tag (lhs)
  lhs :: Exp a -> Exp TAG

  -- | corresponding tag of a variant (rhs)
  rhs :: Variant -> TAG

class Match a where

  match :: Exp a -> [Exp b] -> Exp a



match2 :: Matching f => f -> f
match2 f = mkFun (mkMatch f) Prelude.id

data Args f where
  (:->)  :: Exp a -> Args b -> Args (Exp a -> b)
  Result :: Args (Exp a)

class Matching a where
  type ResultT a
  mkMatch :: a -> Args a -> Exp (ResultT a)
  mkFun   :: (Args f -> Exp (ResultT a))
          -> (Args a -> Args f)
          -> a

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
  mkMatch f (x@(Exp (SmartExp Match {})) :-> xs) = mkMatch (f x) xs -- nested call
  mkMatch f (x@(Exp p)                   :-> xs) = case labels of
             [(_,r)] -> Exp r
             _       -> Exp (SmartExp (Case matchable labels))
    where matchable = SmartExp (Pair (unExp (lhs x)) (SmartExp Nil))
          labels    = Prelude.map (\variant -> (TagRtag ((rhs @e) variant) TagRunit, unExp (mkMatch (f (Exp (SmartExp (Match ((temp @e) variant) p)))) xs))) (variants @e)


-- | generates all resulting functions from all possible patterns.
functions :: forall a b. (Elt a, Elt b) => (Exp a -> Exp b) -> [Exp a -> Exp b]
functions f = Prelude.map (\tag (Exp input) -> f (Exp $ SmartExp $ Match tag input)) (tagsR @a)

-- | generates all resulting functions from all possible patterns, with their corresponding tag.
functionsT :: forall a b. (Elt a, Elt b) => (Exp a -> Exp b) -> [(TagR (EltR a), Exp a -> Exp b)]
functionsT f = Prelude.map (\tag -> (tag, \(Exp input) -> f (Exp $ SmartExp $ Match tag input))) (tagsR @a)

-- | general switch statement
switch :: forall a. (Elt a) => [(TAG, Exp a)] -> (Exp TAG -> Exp a)
switch xs tag     = Exp (SmartExp (Case matchable labels))
  where matchable = SmartExp (Pair (unExp tag) (SmartExp Nil))
        labels    = Prelude.map (\(t, r) -> (TagRtag t TagRunit, unExp r)) xs

-- | generic switch expression
switchT :: forall a b. (Elt a, Elt b) => (Exp a -> Exp TAG) -> [(TAG, Exp a -> Exp b)] -> (Exp a -> Exp b)
switchT tag xs input = Exp (SmartExp (Case matchable labels))  
  where matchable   = SmartExp (Pair (unExp (tag input)) (SmartExp Nil))
        labels      = Prelude.map (\(t, f) -> (TagRtag t TagRunit, unExp (f input))) xs


toTag :: Exp (Maybe Int) -> Exp TAG
toTag (Exp e) = Exp (SmartExp (Prj PairIdxLeft e))

example0 :: Exp (Maybe Int) -> Exp (Int, Int)
example0 = switchT toTag (Prelude.zip [0..] (functions example2))

example1 :: Exp (Maybe Int) -> Exp (Int, Int)
example1 Nothing_       = constant (2, 5)
example1 (Just_ n)      = T2 (5 + (n * 2)) n
example1 _              = T2 (3 * 10) 3

example2 :: Exp (Maybe Int) -> Exp (Int, Int)
--example2 (Variant 0 e)                = T2 3 3
--example2 (Variant 1 e)                = T2 3 10
--example2 (Exp (SmartExp (Match (TagRtag 0 v) e))) = T2 1 0 
--example2 (Exp (SmartExp (Match (TagRtag 1 v) e))) = T2 5 2 
example2 _                                        = T2 3 9


example3 :: Exp (Maybe Bool) -> Exp (Int, Int)
example3 Nothing_       = constant (2, 5)
example3 (Just_ True_)  = constant (5, 3)
example3 _              = T2 (3 * 10) 3

example4 :: Exp Int
example4 = switch [(0, constant 1 :: Exp Int), (1, constant 20 :: Exp Int)] (constant 0)


tags :: forall e. Elt e => Exp e -> [TagR (EltR e)]
tags e = tagsR @e

weird :: Exp (Maybe Int)
weird = Exp (SmartExp (Pair (unExp (constant 50 :: Exp TAG)) (SmartExp (Pair (SmartExp Nil) (unExp (constant 5 :: Exp Int))))))


