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
    CustomBitSize a      = BitSize a

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

traverse :: Monoid r => (forall v. Type v -> r) -> Structure e -> r


---------------------------------------------------------------------------

type Insert value expression   = value -> expression -> expression
type Retrieve value expression = expression -> value 

traverse :: Monoid r => Structure a 
                     -> (forall v. Type v -> Insert v e -> Retrieve v e -> r) 
                     -> Insert a e 
                     -> Retrieve a e 
                     -> r      

----------------------------------------------------------------------------------

data Field e = forall v. Field (Type v) (Insert v e) (Retrieve v e)

fields :: Structure e -> [Field e]
fields = traverse (\type insert retrieve -> [Field type insert retrieve])

-----------------------------------------------------------------------------------

decisions :: forall v u. (Elt v, Elt u) => (Exp v -> Exp u, Exp u -> Exp v) 
decisions = ...

construct :: forall v u. (Elt v, Elt u) => Exp u -> Exp t
construct = fst (decisions @v @u)

destruct :: forall v u. (Elt v, Elt u) => Exp t -> Exp u
destruct = snd (decisions @v @u)


-----------------------------------------------------------------

data Collection (types :: [*]) = VariantWise ...
                               | ElementWise [Variant Compact types]



--------------------------------------------------------------------

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = fold (+) 0 (zipWith (*) xs ys)


------------------------------------------------------------------------

data Maybe a = Just a | Nothing

instance Elt Maybe a where
    type EltR Maybe a = (TAG, a)

fmap :: (Exp a -> Exp b) -> Exp (Maybe a) -> Exp (Maybe b)
fmap f (T2 tag value ) = ...


--------------------------------------------------------------------------

pattern Just_ x <- (T2 1 x)
    where Just_ x = T2 1 x

fmap :: (Exp a -> Exp b) -> Exp (Maybe a) -> Exp (Maybe b)
fmap f (Just_ a) = Just_ (f a)
fmap f Nothing_ = Nothing_

-----------------------------------------------------------------------------

pattern Con :: (Elt (IX n vs), Elt (f vs)) => Exp (IX n vs) -> Exp (V f vs)
pattern Con v <- (matchable (toWord @n) -> Just v)
    where Con v = constructable (toWord @n) (construct v :: Exp (f vs))

type Maybe a = Variant Compact [a, ()]

fmap :: (Exp a -> Exp b) -> Exp (Maybe a) -> Exp (Maybe b)
fmap f (Con0 a)  = Con0 (f a)
fmap f (Con1 ()) = Con1 ()

------------------------------------------------------------------------------

type Primitive = V Compact [Sphere, Plane, Triangle]

intersect :: Exp Primitive -> Exp Pos -> Exp Dir -> Exp Distance
intersect (Con0 sphere)   = sphereIntersect sphere
intersect (Con1 plane)    = planeIntersect plane
intersect (Con2 triangle) = triangleIntersect triangle 

--------------------------------------------------------------------

data Product (constructor :: * -> *) (type :: *)


-----------------------------------------------------------------------

data TagR a where
    TagRtag    :: TAG -> TagR a -> TagR (TAG, a)
    TagRunit   :: TagR ()
    TagRsingle :: a -> TagR a
    TagRpair   :: TagR a -> TagR b -> TagR (a, b)



---------------------------------------------------------------------------

map :: (Variant f as -> Variant f bs) -> Collection f as -> Collection f bs

map :: (Variant as -> Variant bs) -> Collection cs -> Collection (Union cs as)
map = undefined

