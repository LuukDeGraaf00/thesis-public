{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Minimal where

import GHC.Generics
import Data.Typeable (Typeable)
import qualified Data.TMap as Map
import qualified Data.Maybe as M

import qualified Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate as A
import Data.Array.Accelerate.AST                                    ( PrimFun(..) )
import Data.Array.Accelerate.Representation.Array                   ( ArrayR(..) )
import Data.Array.Accelerate.Representation.Shape                   ( ShapeR(..) )
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Smart                                  hiding ( arraysR )
import Data.Array.Accelerate.Sugar.Array                            ( Arrays(..), Array, Scalar, Segments, arrayR )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Foreign
import Data.Array.Accelerate.Sugar.Shape                            ( Shape(..), Slice(..), (:.) )
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Representation.Array         as R


-- | storage of an 'elt' value
data Storage entity where

    Chunk :: (Shape shape, Elt entity) => Acc (Array shape entity) -> Storage entity

-- | constraint that specifies the allowed operations
class Constraint constraint where

    -- | general collection
    data Collection constraint :: *

    -- | insert an embedded 'elt' array independent of the constraint 
    insert :: (Shape sh, Elt e, Typeable e) => Acc (Array sh e) -> Collection constraint -> Collection constraint

    -- | retrieves all relevant chunks 
    chunks :: (Elt e, Typeable e) => Collection constraint -> [Storage e]


-- | instances
data Free = Free

-- | single instance of elt
data Single a

-- | general 
instance Constraint Free where

    -- | dependent map
    data Collection Free = Collection Map.TMap

    -- | free insert
    insert :: (Shape sh, Elt e, Typeable e) => Acc (Array sh e) -> Collection Free -> Collection Free
    insert values (Collection map) = Collection (Map.alter f map)
        where f Nothing       = Just [Chunk values]
              f (Just chunks) = Just (Chunk values : chunks)

    -- | chunks
    chunks :: forall e. (Elt e, Typeable e) => Collection Free -> [Storage e]
    chunks (Collection map) = M.fromMaybe [] (Map.lookup map :: Maybe [Storage e])


-- | storage of a single 'elt'
instance (Elt a) => Constraint (Single a) where

    data Collection (Single a) = Single (Acc (Array DIM1 Int8))

    insert :: (Elt a, Shape sh, Elt e, Typeable e) => Acc (Array sh e) -> Collection (Single a) -> Collection (Single a)
    insert = undefined
  
    chunks :: (Elt a, Elt e, Typeable e) => Collection (Single a) -> [Storage e]
    chunks = undefined

-- | converts to a singular array: can introduce branching and avoids elements within a tag environment
array :: forall e c. (Elt e, Typeable e, Constraint c) => Collection c -> Acc (Vector e)
array collection = foldr (\(Chunk a) b -> flatten a A.++ b) emptyA (chunks collection :: [Storage e])

-- | efficient map 
map :: forall e a b c. (Typeable a, Elt a, Elt b, Constraint c) => (Exp a -> Exp b) -> Collection c -> Collection c
map function collection = undefined
    where r = chunks collection :: [Storage a]


amount :: [Storage e] -> Exp Int
amount = foldr (\(Chunk a) b -> A.size a + b) (constant 0)

emptyA :: (Elt e) => Acc (Vector e)
emptyA = use (fromList (Z:.0) [])

ta :: Acc (Vector Float)
ta = array (insert (use xs) (insert (use xs) (insert (use xs) emptyC)))

-- | 


emptyC :: Collection Free
emptyC = Collection mempty

xs :: Vector Float
xs = fromList (Z:.10) [0..] :: Vector Float

ys :: Vector ()
ys = fromList (Z:.10) [(),(),(),(),(),(),(),(),(),()] :: Vector ()


constructe :: TypeR a -> SmartExp a
constructe TupRunit       = SmartExp Nil
constructe (TupRsingle v) = SmartExp (Undef v)
constructe (TupRpair a b) = SmartExp (Pair (constructe a) (constructe b))



-- | deconstructs existing 'elt' array into a collection
-- deconstruct :: (Arrays arrays) => Collection -> (TupR ArrayR) arrays -> arrays -> Collection
--deconstruct collection TupRunit       ()         = collection
--deconstruct collection (TupRsingle v) values     = Map.insert (Other :: Chunk Float) collection
--deconstruct collection (TupRpair r1 r2) (a1, a2) = collection

-- ?


create :: Array DIM1 Element
create = fromList (Z:.1) [Fire 0]

create2 :: Array DIM1 Element
create2 = fromList (Z:.1) [toElt (1,(((((),0),2),0),0))]

create3 :: Acc (Array DIM1 Element)
create3 = undefined
    where tag   = use (fromList (Z :. 5) [0..1] :: Array DIM1 Word8)
          value = use (fromList (Z :. 5) [0..1] :: Array DIM1 Int8)

create4 :: Acc (Array DIM1 Example)
create4 = use (fromList (Z:.1) (Prelude.map toElt [((), 0)]))

create5 :: Acc (Array DIM1 Example)
create5 = A.map (const toType) (use xs)

toType :: forall a. Elt a => Exp a
toType = Exp (constructe (eltR @a))

testa :: forall a. Elt a => a
testa = error (show (eltR @a))

testb :: forall a. Arrays a => a
testb = error (show (arraysR @a))

testc :: Acc (Vector Element)
testc = A.map (\(Exp (SmartExp f)) -> Exp (SmartExp (teste f))) (use create)

teste :: PreSmartExp SmartAcc SmartExp a -> PreSmartExp SmartAcc SmartExp a
teste other@(Tag a b) = error (show a) 
teste other           = other

mapTest :: (Elt a) => (Exp a -> Exp a) -> Acc (Vector a) -> Acc (Vector a)
mapTest f = A.map (match f) . A.map (match f)


ja :: Exp Element -> Exp Element
ja = Prelude.id

newtype Example = Example Float
    deriving (Show, Generic)

instance Elt Example

data Element = Oil !Int8 | Fire !Int8 | Water !Int8 | None !Int8
  deriving (Show, Generic)

instance Elt Element 

    

    


