{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Debug where
import GHC.Int
import GHC.TypeLits


class (Elt a) where
    type EltR a :: *
    toElt :: a -> EltR a
    fromElt :: EltR a -> a


type family BitSize (a :: *) :: Nat where
    BitSize Bool = 1
    BitSize Int  = 4
    BitSize a    = TypeError (Text "Unsupported type!")

