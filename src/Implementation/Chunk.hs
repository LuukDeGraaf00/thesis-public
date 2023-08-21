module Implementation.Chunk where

import Data.Array.Accelerate as A

{- 

A chunk is a singular collection:

    1. array with single uniform type
    2. array with ungrouped union type (tag)
    3. array with grouped union type (segment)

Special case: nested types for segment descriptors

Supported functionality:

    1. map / imap

-}


data Chunk a = Undefined | Or













