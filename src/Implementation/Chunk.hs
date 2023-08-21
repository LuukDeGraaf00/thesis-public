module Implementation.Chunk where

import Data.Array.Accelerate as A

{- 

Chunk
    1. singular array of one or more types (segment descriptors)
    2. functions operate directly on chunks

Archetype
    1. unique identifier 

Collection
    1. map/dictionary with all chunks
    2. query represents a collection of chunks

Type Protections
    1. Datatype
        - one chunk for each atomic datatype
        - equal length enforced
        - all elements remain equidistant/parallel
        - structural changes must be collective
        - optional array with indices (constant key)
        - optional array with tags (ungrouped)
    2. Nominal
        - restrict structural changes to predetermined set of types


Challenges:
    - iteration offset for composite datatypes
    - span functions over multiple 'arrays' and 'segments'
    - functions have access to index
    - introduce branching for when tags are used
-}

-- | Internal key that represents a type.
-- | All types have an associated index array (maybe possible to omit)
-- | Composite datatypes are constrained to (parallel) equidistance
-- | A collection of composite datatypes must be of a single chunk for each 'primitive' type.
-- | Elements of 'declared' types are constrained to type preserving operations. 
-- | Type synonyms (?)

data Type = Structural      -- independent structural changes  
          | Nominal         -- structural changes restricted to top-level with a fixed-set of types


-- | Chunk
newtype Chunk = Chunk [Segment] (DIM1 Int)
type Segment  = (Type, Int, Int)










