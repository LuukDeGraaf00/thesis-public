{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Store where


import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Representation.Tag
import GHC.Base (Constraint)
import Data.Array.Accelerate.Unsafe (coerce)

{-

Currently Elt class is used to determine the layout of datatypes.
This is done in a snoc-like format... see 'Accelerating Sum Types'.
As it is heavily integrated within Accelerate, the functioning of the Elt class must be preserved.
For sum types, the Elt class is not flexible enough as it is restricted to a single implementation.
Complex pattern matching is also problematic as this will be done in between fused operations that pattern match.
A possible solution is to make an explicit 'store' operation (?).

-}

-- | generic store operation
store :: forall a. (Elt a) => Acc (Array DIM1 a) -> Acc (Array DIM1 a)
store = error (show x Prelude.++ show b)
    where x = eltR @a
          b = bytesElt (eltR @a)
          --c = identity (eltR @a) undef


data Point = Point_ Float Float
  deriving (Generic, Elt)


class (Elt container, Elt variant) => Container container variant where

  -- | constructs a container through a variant
  construct :: Exp variant -> Exp container

  -- | deconstruct container and transform into variant
  destruct :: Exp container -> Exp variant

  -- | tag of variant within the container instance
  tag :: TagR (EltR container)

-- | generic pattern that creates a pattern synonym for sum types
pattern Sum :: forall v c. (Container c v) => Exp v -> Exp c
pattern Sum vars <- (destruct . (matching @c) (tag @c @v) -> vars)
  where Sum = construct

pattern Justy :: Exp Int -> Exp (Maybe Int)
pattern Justy x = Just_ x


--pattern Just2 :: (Elt a) => Exp a -> Exp (Maybe a)


pattern Type abstract concrete <- Exp (SmartExp (Match abstract concrete))


pattern MatchTag :: TAG -> TagR a
pattern MatchTag value <- TagRtag value TagRunit

pattern Nested :: Bool -> Bool
pattern Nested bool <- bool

hel (Nested True)  = False
hel (Nested False) = True
hel _              = error "False"

dah = TagRtag 1 (TagRpair TagRunit (TagRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))))


huh :: Exp Tuple -> Exp Int
huh (JustInt    a) = a
huh (NothingInt b) = 0


{-# COMPLETE Justy, Nothing_ #-}

hud :: Exp (Maybe Int) -> Exp Int
hud (Just_  a) = a
hud Nothing_   = 0

hus :: Exp (Maybe Int) -> Exp Int
hus (Justy  a) = a
hus Nothing_   = 0

hul :: Exp (Maybe Int) -> Exp Int
hul (Exp (SmartExp (Match (TagRtag 1 (TagRpair TagRunit (TagRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))))) e))) = Exp (SmartExp (Prj PairIdxRight (SmartExp (Prj PairIdxRight e))))
hul (Nothing_) = 0


huk :: Exp Word8 -> Exp Word8
huk (Type (MatchTag 5) e) = Exp e
huk (Type (MatchTag 4) e) = Exp e
huk _                     = constant 9


test (-1) = 5


f :: Exp (Maybe Int) -> Exp Int
f (Exp (SmartExp (Match htag e))) = Exp (SmartExp (Prj PairIdxRight (SmartExp (Prj PairIdxRight e))))
f _                               = error "Oh no"

tags = tagsR @(Maybe Int)

htag :: TagR (TAG, ((), Int))
htag = TagRtag 1 (TagRpair TagRunit (TagRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))))

pattern JustInt :: Exp Int -> Exp Tuple
pattern JustInt x = Sum x

pattern NothingInt :: Exp () -> Exp Tuple
pattern NothingInt x = Sum x

matching :: forall e. TagR (EltR e) -> Exp e -> Exp e
matching tag e = Exp (SmartExp (Match tag (unExp e)))


data Tuple = Tuple

instance Elt Tuple where

  type EltR Tuple = (TAG, Int)

  eltR :: TypeR (EltR Tuple)
  eltR = TupRpair (eltR @TAG) (eltR @Int)

  tagsR :: [TagR (EltR Tuple)]
  tagsR = [TagRtag 0 (TagRsingle int), TagRtag 1 (TagRsingle int)]
    where int = SingleScalarType (NumSingleType (IntegralNumType TypeInt))

  toElt n = undefined

  fromElt n = undefined

instance Container Tuple Int where

  construct :: Exp Int -> Exp Tuple
  construct e = coerce (T2 (0 :: Exp TAG) e)

  destruct :: Exp Tuple -> Exp Int
  destruct (Exp e) = Exp (SmartExp (Prj PairIdxRight e))

  tag :: TagR (EltR Tuple)
  tag = TagRtag 0 (TagRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt))))

instance Container Tuple () where

  construct :: Exp () -> Exp Tuple
  construct e = coerce (T2 (1 :: Exp TAG) (undef :: Exp Int))

  destruct :: Exp Tuple -> Exp ()
  destruct (Exp e) = Exp (SmartExp Nil)

  tag :: TagR (EltR Tuple)
  tag = TagRtag 1 (TagRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt))))



data layout > (types :: [*]) where


instance (Elt x) => Elt (x > '[]) where

  type EltR (x > '[]) = (TAG, EltR x)

  eltR :: Elt x => TypeR (EltR (x > '[]))
  eltR = undefined

  tagsR = undefined

  toElt n = undefined

  fromElt n = undefined

instance (Elt x, Elt (x > ys)) => Elt (x > (y : ys)) where

  type EltR (x > (y : ys)) = EltR (x > ys)

  eltR = eltR @(x > ys)

  tagsR = undefined

  toElt n = undefined

  fromElt n = undefined
