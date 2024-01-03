{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Store where

import qualified Prelude as P

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Representation.Tag
import Variant
import Data.Typeable
import Debug.Trace
import Data.Kind




mapG :: forall a b c. (Sum a, Sum b, Sum c, VariantF a b, VariantF a c) => (Exp b -> Exp c) -> Acc (Array DIM1 a) -> Acc (Array DIM1 a)
mapG function = A.map (construct @a @c . function . destruct @a @b)

mapFlex :: forall a b c d. (Sum a, Sum b, Sum c, Sum d, VariantF a b, VariantF d c) => (Exp b -> Exp c) -> Acc (Array DIM1 a) -> Acc (Array DIM1 d)
mapFlex function = A.map (construct @d @c . function . destruct @a @b)
