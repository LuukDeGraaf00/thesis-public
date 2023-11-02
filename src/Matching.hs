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

{-

Embedded pattern matching involves transforming the Haskell AST to the Accelerate AST.


'Simple Solution' -> generate new datatype





-}

class IsMaybe where

instance IsMaybe => Container a where

  construct = undefined
  deconstruct = undefined

class Container result where

  construct :: e -> Exp result

  deconstruct :: Exp result -> e



class Helper a where

  count :: Int

  array :: [Prelude.String]

instance Helper (a := '[]) where

  count :: Int
  count = 0

  array :: [Prelude.String]
  array = []

instance (Elt x, Helper (a := xs)) => Helper (a := (x : xs)) where

  count :: Int
  count = 1 + count @(a := xs)

  array :: [Prelude.String]
  array = Prelude.show (eltR @x) : array @(a := xs)




oneC :: Exp x -> Exp (context := (x : xs))
oneC a = undefined

twoC :: Exp x -> Exp (context := (z : x : xs))
twoC a = undefined


ta :: Int
ta = count @(Int := [Int, Int, Float])

tb = eltR @(Bool := [Int, Int, Int])

tc = array @(Int := [Int, Int, Maybe Float])


data context := (types :: [*]) where

class Reducible a b where
    
    type Reduce a b

    create :: TypeR (EltR (Reduce a b))


instance (Elt a) => Reducible a a where

    type Reduce a a = a

    create :: TypeR (EltR (Reduce a a))
    create = eltR @a

instance (Elt a) => Reducible a () where

  type Reduce a () = a

  create :: TypeR (EltR (Reduce a ()))
  create = eltR @a

instance (Elt a, Elt b) => Reducible a b where

    type Reduce a b = a

    create :: TypeR (EltR (Reduce a b))
    create = eltR @a


instance Elt (context := '[]) where

  type EltR (context := '[]) = ()

  eltR :: TypeR (EltR (context := '[]))
  eltR = TupRunit
  
  tagsR :: [TagR (EltR (context := '[]))]
  tagsR = []
  
  toElt n = undefined 
  
  fromElt n = undefined

instance (Reducible (EltR x) (EltR (c := xs))) => Elt (c := (x : xs)) where

  -- | generate tagged instance
  type EltR (c := (x : xs)) = (TAG, Reduce (EltR x) (EltR (c := xs)))

  --eltR :: TypeR (EltR (c := (x : xs)))
  --eltR = TupRpair (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord8)))) (create @x @(c := xs))
  
  eltR = undefined

  tagsR = undefined
  
  toElt n = undefined
  
  fromElt n = undefined



-- | generates all resulting functions from all possible patterns.
functions :: forall a b. (Elt a, Elt b) => (Exp a -> Exp b) -> [Exp a -> Exp b]
functions f = Prelude.map (\tag (Exp input) -> f (Exp $ SmartExp $ Match tag input)) (tagsR @a)

-- | generates all resulting functions from all possible patterns, with their corresponding tag.
functionsT :: forall a b. (Elt a, Elt b) => (Exp a -> Exp b) -> [(TagR (EltR a), Exp a -> Exp b)]
functionsT f = Prelude.map (\tag -> (tag, \(Exp input) -> f (Exp $ SmartExp $ Match tag input))) (tagsR @a)

-- | define equality
instance (Elt a) => Prelude.Eq (TagR a) where

  (==) :: Elt a => TagR a -> TagR a -> Bool
  (==) TagRunit       TagRunit       = True
  (==) (TagRsingle a) (TagRsingle b) = True
  (==) _ _                           = False


example1 :: Exp (Maybe Int) -> Exp (Int, Int)
example1 Nothing_       = constant (2, 5)
example1 (Just_ n)      = T2 (5 + (n * 2)) n
example1 _              = T2 (3 * 10) 3

example4 :: Exp (Maybe Int) -> Exp (Int, Int)
example4 (Exp (SmartExp (Match (TagRtag 0 v) e))) = T2 1 0 
example4 (Exp (SmartExp (Match (TagRtag 1 v) e))) = T2 5 2 
example4 (Exp (SmartExp a))                       = error (showPreExpOp a)


example3 :: Exp (Maybe Bool) -> Exp (Int, Int)
example3 Nothing_       = constant (2, 5)
example3 (Just_ True_)  = constant (5, 3)
example3 _              = T2 (3 * 10) 3


tags :: forall e. Elt e => Exp e -> [TagR (EltR e)]
tags e = tagsR @e

weird :: Exp (Maybe Int)
weird = Exp (SmartExp (Pair (unExp (constant 1 :: Exp TAG)) (SmartExp (Pair (SmartExp Nil) (unExp (constant 5 :: Exp Int))))))


