module Implementation.Chunk where

import Data.Array.Accelerate as A

import qualified Data.Map.Lazy as Map

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



-- | Internal key that represents a type.
--newtype Type = Type Int
--    deriving (Prelude.Eq, Prelude.Ord)

--data Chunk = Chunk [Segment] (A.Acc (A.Array DIM1 a))

data Chunk a = Array (A.Acc (A.Array DIM1 a)) 
             | Segments 

              

--type Segment = (Type, Int, Int)

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






