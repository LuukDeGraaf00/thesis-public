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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}

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

import GHC.Word

{-

Embedded pattern matching involves transforming the Haskell AST to the Accelerate AST.


'Simple Solution' -> generate new datatype





-}

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

    Variant :: (Elt e) => Exp e -> V types -> V (e : types)


class Helper a where

  count :: Int

instance Helper (V '[]) where

  count :: Int
  count = 0

instance Helper (V xs) => Helper (V (x : xs)) where

  count :: Int
  count = 1 + count @(V xs)


ta :: Int
ta = count @(V [Int, Int, Float])

tb = eltR @(V [Int, Int, Float])


instance Elt (V '[]) where

  type EltR (V '[]) = ()

  eltR = undefined
  
  tagsR = undefined
  
  toElt n = undefined
  
  fromElt n = undefined

instance Elt (V xs) => Elt (V (x : xs)) where

  type EltR (V (x : xs)) = (x, EltR (V xs))

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



pattern IsJusty :: Elt a => Exp a -> Exp (Maybe a)
pattern IsJusty v <- (\(Exp a) -> Exp (SmartExp (Prj PairIdxRight (SmartExp (Prj PairIdxRight a)))) -> v)
  where IsJusty (Exp v) = Exp (SmartExp (Pair (unExp (constant 1 :: Exp TAG)) (SmartExp (Pair (SmartExp Nil) v))))


class Huh a where

instance (EltR a ~ EltR b) => Huh a where


example2 :: Exp (Maybe Int) -> Exp (Int, Int)
example2 s@(Exp a) = Exp (SmartExp (Case (toTag s) xs))
    where xs = Prelude.map (\tag -> (f tag, unExp (example1 (Exp (SmartExp (Match tag a)))))) (tagsR @(Maybe Int))
          f  = undefined :: TagR (EltR (Maybe Int)) -> TagR (TAG, ())


example3 :: Exp (Maybe Bool) -> Exp (Int, Int)
example3 Nothing_       = constant (2, 5)
example3 (Just_ True_)  = constant (5, 3)
example3 _              = T2 (3 * 10) 3


tags :: forall e. Elt e => Exp e -> [TagR (EltR e)]
tags e = tagsR @e

weird :: Exp (Maybe Int)
weird = Exp (SmartExp (Pair (unExp (constant 1 :: Exp TAG)) (SmartExp (Pair (SmartExp Nil) (unExp (constant 5 :: Exp Int))))))



-- | additional class that 
class Matching a where

    -- computes tag
    mapTag :: TagR (EltR a) -> TagR (TAG, ())

    -- 
    toTag :: Exp a -> SmartExp (TAG, ())



instance Matching (Maybe Int) where

  mapTag :: TagR (EltR (Maybe Int)) -> TagR (TAG, ())
  mapTag (TagRtag t _) = TagRtag t TagRunit
  mapTag _             = error "huh"

  toTag :: Exp (Maybe Int) -> SmartExp (TAG, ())
  toTag (Exp e) = SmartExp (Pair (SmartExp (Prj PairIdxLeft e)) (SmartExp Nil)) :: SmartExp (TAG, ())



data Args f where

  (:->)  :: Exp a -> Args b -> Args (Exp a -> b)

  Result :: Args (Exp a)

data A f where

  F :: Exp a -> A b -> A (Exp a -> b)

  R :: A (Exp a)


-- | structural sum type class
class Structural e where

  type Result e

  -- | retrieve a function that 
  construct :: (A f -> Exp (Result e)) -> (A e -> A f) -> e

  deconstruct :: e -> A e -> Exp (Result e)

-- | single instance
instance Elt a => Structural (Exp a) where

  -- | return original type
  type Result (Exp a) = a

  -- | finalize
  construct :: Elt a => (A f -> Exp (Result (Exp a))) -> (A (Exp a) -> A f) -> Exp a
  construct f k = f (k R)

  -- | return
  deconstruct :: Elt a => Exp a -> A (Exp a) -> Exp (Result (Exp a))
  deconstruct (Exp e) R = case e of
      SmartExp (Match _ x) -> Exp x -- ignore match
      _                    -> Exp e -- return original

-- | function instance
instance (Elt a, Structural b) => Structural (Exp a -> b) where

  -- | type family allows for flexible output
  type Result (Exp a -> b) = Result b

  -- | reconstruct function
  construct :: (Elt a, Structural b) => (A f -> Exp (Result (Exp a -> b))) -> (A (Exp a -> b) -> A f) -> (Exp a -> b)
  construct function transform input = construct function (transform . F input)

  -- | provide input to function and traverse further
  deconstruct :: (Elt a, Structural b) => (Exp a -> b) -> A (Exp a -> b) -> Exp (Result (Exp a -> b))
  deconstruct function (F input@(Exp e) result) = case e of

      SmartExp Match{} -> deconstruct (function input) result

      _                -> case rhs of
                            [(_,r)]    -> Exp r                       -- product type
                            (_,r) : xs -> Exp r
                            _          -> Exp (SmartExp (Case e rhs)) -- sum type

    where rhs = [ (tag, unExp (deconstruct (function (Exp (SmartExp (Match tag e)))) result)) | tag <- tagsR @a ]


matchDebug :: Structural f => f -> f
matchDebug f = construct (deconstruct f) Prelude.id


as :: [Acc (Array DIM1 (Int, Int))]
as = Prelude.map (`A.map` list) (functions (match example1))
  where list = use (fromList (Z :. 2) [Just 2, Nothing])



