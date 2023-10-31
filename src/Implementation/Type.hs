{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Implementation.Type where

import Data.Array.Accelerate as A
--import Data.Array.Accelerate.LLVM.PTX as GPU
import Data.Array.Accelerate.Linear as L
import Data.Array.Accelerate.Data.Maybe as M


data Example = One | Two (Maybe Bool)
    deriving (Show, Generic, Elt)

mkPattern ''Example

--instance Elt Example where

{-




-}

