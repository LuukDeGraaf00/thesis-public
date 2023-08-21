{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE InstanceSigs #-}

module Benchmark where

import qualified Prelude as P
import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX as GPU
import Data.Array.Accelerate.Linear as L
import Data.Array.Accelerate.Data.Maybe as M
import Data.Array.Accelerate.Data.Bits as B
import Criterion.Main


-- | entry point for benchmarking
benchmark :: Benchmark -> P.IO ()
benchmark x = defaultMain [x]

-- input

xs :: Acc (Vector Float)
xs = use (fromList (Z:.10) [0..] :: Vector Float)

ys :: Acc (Vector Float)
ys = use (fromList (Z:.10) [1,3..] :: Vector Float)

-- indexing overhead

indexing :: Benchmark
indexing = bgroup "indexing overhead"
    [bench "normal     " (whnf GPU.run (dotp1 xs ys)),
     bench "index      " (whnf GPU.run (dotp2 xs ys)),
     bench "integer    " (whnf GPU.run (dotp3 xs ys)),
     bench "reverse    " (whnf GPU.run (dotp4 xs (A.reverse ys))),
     bench "match      " (whnf GPU.run (dotp5 xs (A.map Just_ ys))),
     bench "conditional" (whnf GPU.run (dotp6 xs ys))]

dotp1 :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp1 xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

dotp2 :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp2 xs ys = A.fold (+) 0 (A.imap (\i x -> x * (ys A.! i)) xs)

dotp3 :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp3 xs ys = A.fold (+) 0 (A.imap (\(I1 i) x -> x * (ys A.!! i)) xs)

dotp4 :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp4 xs ys = A.fold (+) 0 (A.imap (\(I1 i) x -> x * (ys A.!! (999 - i))) xs)

dotp5 :: Acc (Vector Float) -> Acc (Vector (Maybe Float)) -> Acc (Scalar Float)
dotp5 xs ys = A.fold (+) 0 (A.zipWith (*) xs (A.map (fromMaybe (constant 1)) ys))

dotp6 :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp6 xs ys = A.fold (+) 0 (A.zipWith (\x y -> x A.== y ? (y * x + 0.1, x * y)) xs ys)