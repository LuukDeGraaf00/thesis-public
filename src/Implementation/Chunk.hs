{-# language GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Implementation.Chunk where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Type as T
import Data.Array.Accelerate.Interpreter
import qualified GHC.Generics


{-  

Chunk
    1. singular array of one or more types (segment descriptors)
    2. functions operate directly on chunks

Archetype
    1. unique identifier 

Collection
    1. map/dictionary with all chunks
    2. query represents a collection of chunks

Constraints
    1. Datatype
        - one chunk for each atomic datatype
        - equal length enforced
        - all elements remain equidistant/parallel
        - structural changes must be defined on the top-level
        - optional array with tags (ungrouped)
    2. Nominal
        - restrict structural changes to predetermined set of types
        - restrict structural changes to top level functions
    3. Indexed
        - optional array with indices (constant key)
    4. ?
        - functions also operate on 'subsets' that contain a datatype


Challenges:
    - iteration offset for composite datatypes
    - span functions over multiple 'arrays' and 'segments'
    - functions have access to index
    - introduce branching for when tags are used
-}

-- | example
data Chunk value where

    -- | single
    Array :: (T.IsScalar value) => Chunk value

    -- | segmented
    Segmented :: (T.IsScalar value) => value || segment -> Chunk (value || segment)


-- | 

data a |: b where

-- | segmented
infixr 3 ||
infixr 3 :+:
infixr 3 :*:

data value || segment where

    -- | segment
    (:+:) :: (T.IsScalar value, T.IsScalar other, BitSizeEq value other) => Int -> other || segment -> value || (other || segment)

    -- | final
    (:*:) :: (T.IsScalar value, T.IsScalar segment, BitSizeEq value segment) => Int -> Int -> value || segment


    

-- | tagged 

example :: Float || Float || Int32
example = 5 :+: 10 :*: 100

test :: Acc (Array (Z :. Int) Int32)
test = use (fromList (Z :. 1000) [0..100000])

tester :: Acc (Array (Z :. Int) Int32)
tester = A.slit (constant 0) (constant 100) test

adf :: Acc (Array (Z :. Int) Word32)
adf = A.map bitcast test

type Values value = A.Acc (A.Array (DIM0 :. Int) value) 


-- | wrapper around an array of data
class Storage storage where

    -- | collection that stores in a particular way
    data Collection storage :: * -> *

    -- | iterates over the collection on a certain value
    map :: (Elt a, Elt b) => (a -> b) -> Collection storage c -> Collection storage d


instance Storage Int where

    data Collection Int v = Test

    map :: (Elt a, Elt b) => (a -> b) -> Collection Int c -> Collection Int d
    map f xs = undefined

