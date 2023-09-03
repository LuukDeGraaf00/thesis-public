{-# LANGUAGE TypeFamilies #-}
module Implementation.Map where

import Data.Array.Accelerate as A

import Implementation.Chunk

import qualified Data.Map.Lazy as Map
import Data.Array.Accelerate.Sugar.Elt

{-
    https://kowainik.github.io/posts/2018-07-11-typerep-map-step-by-step

    type-indexed map implementation
-}

class Datatype a where

    -- | collection of datatype
    data Collection a :: *

    -- | identifies all relevant chunks through a collection
    chunks :: Elt b => Collection a -> [Chunk b]


-- | creates an array of all chunks that contain a certain elt
lookup :: (Datatype a, Elt b) => Collection a -> Acc (Array DIM1 b)
lookup collection = merge (chunks collection)

-- | updates the data
map :: Elt a => Collection a -> (Exp a -> Exp a) -> Collection a
map = undefined

-- | modifies the structural identify of data
modify :: (Elt a, Elt b) => Collection a -> (Exp a -> Exp b) -> Collection a
modify = undefined

-- | merges all chunks into a singular array for use
merge :: Elt a => [Chunk a] -> Acc (Array DIM1 a)
merge = undefined
