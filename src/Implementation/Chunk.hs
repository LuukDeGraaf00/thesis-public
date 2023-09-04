{-# language GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Implementation.Chunk where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Type as T


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


-- | any elt variable is queryable
class (T.IsScalar value) => Queryable value where

-- | meta data
class Meta meta where

-- | storage of chunks
class Collection meta where

    -- | definition function
    data Col meta :: * -> *

    -- | find all chunks through a meta constraint
    lookup :: (Queryable value) => Col meta value -> [Chunk value]

    -- | insert chunk
    insert :: (Queryable value) => [Chunk value] -> Col meta value  -> Col meta value



data Example = Tree

instance Collection Example where

  lookup :: Queryable value => Col Example value -> [Chunk value]
  lookup = undefined

  insert :: Queryable value => [Chunk value] -> Col Example value -> Col Example value
  insert = undefined

instance Meta Example where



-- | underlying data structure
data Chunk value where

    -- | a singular array of a scalar value
    Chunk :: Queryable value => A.Acc (A.Array DIM1 value) -> Chunk value



-- | segment within array
type Segment a = (a, Int, Int)




{-



data GenericChunk = Float (Chunk Float)
                  | Int   (Chunk Int)

example :: Acc (Vector Float) -> Acc (Vector Float)  -> Collection
example xs ys = Map.fromList 
    [
        (Type 0, Float (Chunk [] xs)),
        (Type 1, Float (Chunk [] ys))
    ]

dotp7 :: Collection -> Acc (Scalar Float)
dotp7 c = A.fold (+) 0 (A.zipWith (*) xs ys)
    where (Just (Float (Chunk [] xs))) = Map.lookup (Type 0) c
          (Just (Float (Chunk [] ys))) = Map.lookup (Type 1) c
-}
