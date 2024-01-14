{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-}


module Implementation where

import TypeLevel
import Variant
import Generics
import Data.Array.Accelerate.Sugar.Elt
import Data.Typeable


-- | compact sum type representation
type CV (types :: [r]) = Variant (Constructor CRep) types

-- | compact representation
type Compact (types :: [r]) = NatType (MaximumBitSize 0 types)

-- | key used for dictionary
data CRep

-- | dictionary instance for application of a compact representation
newtype instance Constructor CRep types = CRep (Compact types)

-- | elt instance
instance (Elt (Compact types)) => Elt (Constructor CRep types) where

    type EltR (Constructor CRep types) = EltR (Compact types)

    eltR = eltR @(Compact types)

    tagsR = tagsR @(Compact types)

    fromElt = undefined 

    toElt = undefined


daf = show $ eltR @(CV [Float, Float, Maybe Float])
dah = show $ tagsR @(CV [Float, Int, Maybe Float])