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

-- | meta information that is used to define a chunk
class Info info where

-- | storage of data with meta information
data Chunk info value where

    -- | an array
    Array :: (Info info, Queryable value) => info -> A.Acc (A.Array DIM1 value) -> Chunk info value

    -- | segmented collection
    Segmented :: (Info info, Queryable value) => [Segment info] -> A.Acc (A.Array DIM1 value) -> Chunk info value

    -- | tagged collection
    Tagged :: (Info info) => Chunk info Int 


data Test = Test


-- | collection with certain categorization
class (Info info) => Storage info where

    -- | collection instance for each meta information
    data Collection info :: * -> *


    -- 
    --chunks :: Queryable value => Collection meta -> Chunk meta value

-- | query decides which chunks are relevant to be returned
class Query query where

    -- | return all relevant chunks
    valid :: (Info info) => query -> info -> Bool


-- | segment within array
type Segment meta = (meta, Int, Int)




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
