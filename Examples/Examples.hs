type Ray       = Float
type Triangle  = Float


-- intro

--------------------------------------------------------------

nearest :: Ray -> [Triangle] -> Float
nearest ray = fold min 1e30 . map (intersect ray)

--------------------------------------------------------------

data Primitive = Triangle ___ | Sphere ____

nearest :: Ray -> [Primitive] -> Float
nearest ray = fold min 1e30 . map (intersect ray)

--------------------------------------------------------------

class Primitive a where
    intersect :: Ray -> a -> Float

nearest :: (Primitive a) => Ray -> [[a]] -> Float
nearest ray = fold min 1e30 . fold (map (intersect ray)) 1e30

--------------------------------------------------------------

-- value-level tagged union with a closed system
data Primitive = Triangle ___ | Sphere ____

-- type-level interface with an open system
class Primitive a where
    intersect :: Ray -> a -> Float

---------------------------------------------------------------



data Maybe a = Just a | Nothing



---------------------------------------------------------------

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap f (Just a) = Just (f a)
fmap f Nothing  = Nothing
    



---------------------------------------------------------------

fmap :: (a -> b) -> Just a -> Just b
fmap f (Just a) = Just (f a)

divide :: Int -> Int -> (Just Int || Nothing)
divide _ 0 = Nothing
divide n m = Just (n `div` m)


---------------------------------------------------------------


2.5          :: Float               -- type
Float        :: *                   -- kind
Option a     :: * -> *              -- kind
Option Float :: *                   -- kind
Apply f a    :: (* -> *) -> * -> *  -- kind


----------------------------------------------------------------

-- open universe where ’a’ can be any type
data Succ a
data Nil

-- closed universe under the phantom type ’a’
data Natural a where
    Succ :: Natural b -> Natural (Natural b)
    Nil  :: Natural ()

-----------------------------------------------------------------


data Natural = Succ Natural | Nil | Add Natural Natural

---------------------------------------------------------------------------------------

class (Elt a) where
    type EltR a :: *
    toElt       :: a -> EltR a
    fromElt     :: EltR a -> a

---------------------------------------------------------------------------------

type family Elem a (bs :: [b]) :: Bool where
    Elem x '[]       = False        -- no
    Elem x (x : ys)  = True         -- yes
    Elem x (y : ys)  = Elem x ys    -- no, but recurse

-------------------------------------------------------------------------------

data Variant (constructor :: [variant] -> *) (variants :: [variant])

--------------------------------------------------------------------------------

data family Constructor argument :: [variant] -> *

type V argument (variants :: [variant]) = Variant (Constructor argument) variants

----------------------------------------------------------------------------------

type family BitSize (a :: *) :: Nat where
    BitSize Word8  = 8
    BitSize Custom = ?

-----------------------------------------------------------------------------------

class (Elt a) where
    type EltR a :: *
    eltR        :: TupR (EltR a)

data TupR v where
    TupRunit   ::                     TupR ()
    TupRsingle :: a                -> TupR a
    TupRpair   :: TupR a -> TupR b -> TupR (a, b)

----------------------------------------------------------------------------------------

type SizeInBits a = CustomBitSize (EltR a)

type family CustomBitSize (a :: *) :: Nat where
    CustomBitSize (a, b) = CustomBitSize a + CustomBitSize b 
    CustomBitSize ()     = 0                   
    CustomBitSize a      = CustomBitSize a

-----------------------------------------------------------------------------------------------

type family IntermediateRep (a :: *) :: [Nat] where
    IntermediateRep (a, b) = IntermediateRep a ++ IntermediateRep b 
    IntermediateRep ()     = '[]                   
    IntermediateRep a      = BitSize a : '[]


-------------------------------------------------------------------------------------------

type family Difference (a :: [r]) (b :: [r]) :: [r] where
    Difference '[]      bs = bs
    Difference (a : as) bs = a : Difference as (RemoveOne a bs)

type family Union (a :: [Nat]) (b :: [r]) :: [Nat] where
    Union xs  '[]      = xs
    Union xs  (y : ys) = Union (Difference xs (SizeInBits y)) ys

------------------------------------------------------------------------------------

type family Sort (types :: [Nat]) :: [Nat] where
    Sort '[]      = '[]
    Sort (x : xs) = Insert x (Sort xs)


-----------------------------------------------------------------------------------


class (Elt a) where
    type EltR a :: *

instance Elt (constructor types) => Elt (Variant constructor types) where

-------------------------------------------------------------------------

class (Elt v, Elt u, SizeInBits v <= SizeInBits u) => IsVariant v u where

    construct :: Exp v -> Exp u
    construct = ...

    destruct  :: Exp u -> Exp v
    destruct = ...


---------------------------------------------------------------------------

data Color = Red | Green | Blue 

transform :: Color -> Color
transform Red   = ...
transform Green = ...
transform Blue  = ...


class ColorInterface a 
    transform :: a -> a

instance ColorInterface Red where
    transform :: Red -> Red
    transform = ...
---------------------------------------------------------------------------

-- HELPERS

fold :: (a -> a -> a) -> a -> [b] -> a
fold = undefined

intersect :: a -> b -> c
intersect = undefined
