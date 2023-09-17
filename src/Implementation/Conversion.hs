module Implementation.Conversion where

import Data.Array.Accelerate as A

{-

The Elt class is a mapping to primitive values, which needs to be preserved for legacy reasons.
A wrapper around this is not powerful enough as it does not allow for flexibility around the indices, as it conflicts with the Shape constraint.



-}
