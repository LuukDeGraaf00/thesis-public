{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}


module Debug where

data Nat = Succ Nat | Nil

data Add (a :: Nat) (b :: Nat) where

    Plus :: Add (a :: Nat) (a :: Nat)
