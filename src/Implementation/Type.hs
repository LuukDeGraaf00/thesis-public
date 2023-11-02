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
import Data.Array.Accelerate.Data.Maybe as M
import Data.Array.Accelerate.Sugar.Elt


data Example = One | Two (Maybe Bool)
    deriving (Show, Generic)

instance Elt Example where
  
  eltR = undefined

  tagsR = undefined
  
  toElt n = undefined
  
  fromElt n = undefined


mkPattern ''Example

test :: Exp Example -> Exp Bool
test One_       = undefined
test (Two_ b)   = undefined


--instance Elt Example where

{-




-}

