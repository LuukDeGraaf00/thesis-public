{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Implementation.Map where

import Data.Array.Accelerate as A

import Implementation.Chunk
import Implementation.Type

import Implementation.Conversion

import Data.Functor.Identity (Identity(..))
import Data.Array.Accelerate.Type as T
import Data.Array.Accelerate.Sugar.Elt

{-
    https://kowainik.github.io/posts/2018-07-11-typerep-map-step-by-step

    type-indexed map implementation
-}

xs :: Acc (Vector Float)
xs = use (fromList (Z:.10) [0..] :: Vector Float)


class (Elt value) => Map value where
    
    map :: (Map a, Map b) => (value -> result) -> a -> b


-- | type-indexed map of chunks
--type Collection = Map.DMap Chunk Identity


{-
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
-}

--create :: T.IsScalar a => Acc (Vector a) -> Collection
--create vs = Map.singleton Single (Identity [vs])

--insert ::  T.IsScalar a => Acc (Vector a) -> Collection -> Collection
--insert vs = Map.insert Single (Identity [vs])