{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Store where


import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Unsafe

{-

Currently Elt class is used to determine the layout of datatypes.
This is done in a snoc-like format... see 'Accelerating Sum Types'.
As it is heavily integrated within Accelerate, the functioning of the Elt class must be preserved.
For sum types, the Elt class is not flexible enough as it is restricted to a single implementation.
Complex pattern matching is also problematic as this will be done in between fused operations that pattern match.
A possible solution is to make an explicit 'store' operation (?).

-}

newtype Example = Example Float
    deriving (Show, Generic)

instance Elt Example

data Element = Oil !Int8 | Fire !Int8 | Water !Int8 | None !Int8
  deriving (Show, Generic)

instance Elt Element 


-- | generic store operation
store :: forall a. (Elt a) => Acc (Array DIM1 a) -> Acc (Array DIM1 a)
store = error (show x Prelude.++ show b)
    where x = eltR @a
          b = bytesElt (eltR @a)
          --c = identity (eltR @a) undef


identity :: (Elt e) => Exp e -> Exp e
identity (Exp (SmartExp Nil))                     = Exp (SmartExp Nil)
--identity (Exp (SmartExp (Pair (SmartExp Nil) b))) = error "Newtype"
identity (Exp (SmartExp (Pair a b)))              = Exp (SmartExp (Pair a b))
identity (Exp (SmartExp (Tag t l)))               = error (show t)
identity _                                        = error "WIP"


-- retrieves
apply :: Exp Example -> Exp Float
apply = coerce


applyy :: forall a b. Coerce (EltR a) (EltR b) => Exp a -> Exp b
applyy = coerce

--elter :: forall e. (Elt e) => Exp e -> Exp (EltR e)
--elter (Exp e) = Exp (structural (eltR @e) e)


--un :: Exp Element -> Exp (EltR Element)
--un = coerce

--una :: (Elt e) => Exp e -> Exp (EltR e)
--una = coerce

elter :: forall a b. (Elt a) => SmartExp a -> SmartExp (EltR a)
elter e = case (eltR @a, e) of
  (TupRunit,          a) -> undefined
  (TupRsingle st,     a) -> undefined
  (TupRpair tr tr',   a) -> undefined



-- | empty instance
empty :: SmartExp () -> SmartExp (EltR ())
empty e = SmartExp Nil

-- | single instance
single :: forall a. (Elt a) => SmartExp a -> SmartExp (EltR a)
single = elter

-- | tuple instance
tuple :: forall a b. (Elt a, Elt b) => SmartExp (a, b) -> SmartExp (EltR a, EltR b)
tuple e = SmartExp $ Pair (elter $ SmartExp $ Prj PairIdxLeft e) (elter $ SmartExp $ Prj PairIdxRight e)


structural :: (Elt a) => TypeR a -> b
structural TupRunit              = undefined
structural (TupRsingle value)    = undefined
structural (TupRpair left right) = undefined
    

--ma :: Exp ((), Float)
--ma = structural undefined (constant (Example 5))


-- | mirror representation
--mirror :: (Elt a) => TypeR a -> PreSmartExp acc exp a -> PreSmartExp acc exp a
--mirror TupRunit       Nil      = Nil
--mirror (TupRsingle v) _        = undefined -- Exp t
--mirror (TupRpair a b) _        = undefined -- Exp (SmartExp (Pair (Store.mirror (Exp t) a) (Store.mirror (Exp t) b)))
--mirror _ _ = undefined

ta :: Exp Float -> Exp (Float, Float)
ta (Exp float) = Exp (SmartExp (Pair (SmartExp (Pair (SmartExp Nil) float)) float))

tb :: Exp (Float, Float) -> Exp Float
tb (Exp (SmartExp (Pair (SmartExp (Pair (SmartExp Nil) a)) b))) = Exp b

test :: Exp Float
test = tb ((\(T2 a b) -> T2 b a) (constant (1,2)))


-- | just general mapping to store
storeA :: Acc (Array DIM1 Float) -> Acc (Array DIM1 Float)
storeA = A.map (A.- 1) . A.compute . A.map (A.+ 1)


xs :: Vector Float
xs = fromList (Z:.10) [0..] :: Vector Float

ys :: Vector ()
ys = fromList (Z:.10) [(),(),(),(),(),(),(),(),(),()] :: Vector ()


try :: Arrays a => Acc a -> a
try = I.run