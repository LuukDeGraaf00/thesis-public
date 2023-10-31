{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Implementation.Conversion where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Representation.Tag
import GHC.Generics
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Interpreter (run)
import Debug.Trace

{-

The Elt class is a mapping between values of both AST's, which needs to be preserved for legacy reasons.
The goal is to decouple datatypes and the accompanied storage, introducing flexibility while preserving existing functionality as much as possible.
In the general case there are two ways to store, either collectively or through tags.

As Elt only pertains to an individual way of storage, this is not sufficient for collective based data structures.
While the creation of a new data structure is invasive, it can be mitigated through conversion to an array.
Array fusion can mitigate most performance degradation, but will require branching in all instances where tags are required.
This is presumably less problematic for GPU, as it does not cause divergence due to the spatial locality of branches. 

An initial idea is to use a 'large' backend, while having a smaller 'interface'.


-}


-- idea : (de)-constructing based on functions
-- some kind of mapping for existing pattern matching functions (transform like the 'match' function(?))

-- haskell instance
data Element = Oil !Int8 | Fire !Int8 | Water !Int8 | None !Int8
  deriving (Show, Generic) 


pattern Oils :: Int8 -> Int8
pattern Oils n <- (deconstruct -> Oil n)

pattern Fires :: Int8 -> Int8
pattern Fires n <- (deconstruct -> Fire n)

deconstruct :: Int8 -> Element
deconstruct 0 = Oil  0
deconstruct 1 = Fire 1
deconstruct _ = Fire 2

-- fake to real (?)
testd :: Int8 -> Element
testd (Oils n)  = Oil n
testd (Fires n) = Fire n
testd _         = None 0


class IsType a where

data a || b where

  Type :: (Elt a, Elt b) => a -> a || b

-- accelerate instance
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

-- fake accelerate pattern synonyms (?)

-- definition
--pattern Oil_ :: Exp Int8 -> Exp Element
--pattern Oil_ t = unlift t

testa :: Acc (Array DIM1 Element)
testa = use (fromList (Z :. 5) [Oil 5, Water 2, Fire 3, Fire 10, Water 10])

function :: Element -> Element
function (Oil n)   = Oil   (n + 1)
function (Fire n)  = Fire  (n + 2)
function (Water n) = Water (n + 3)
function (None n)  = None  (n + 4)

functionE :: Exp Element -> Exp Element
functionE n = constant (Oil 0 :: Element)

result :: Array DIM1 Element
result = run testa


data Aligned a where 

  Aligned :: (Elt a) => a -> Aligned a


instance (Elt a) => Elt (Aligned a) where

    type EltR (Aligned a) = Int8

    eltR :: TypeR (EltR (Aligned a))
    eltR = TupRsingle scalarType
    
    tagsR :: [TagR (EltR (Aligned a))]
    tagsR = [TagRsingle scalarType]
    
    toElt :: EltR (Aligned a) -> Aligned a
    toElt n = Aligned undefined
    
    fromElt :: Aligned a -> EltR (Aligned a)
    fromElt n = undefined