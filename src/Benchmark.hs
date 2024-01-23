{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Benchmark where

import qualified Prelude as P
import Data.Fixed
import Data.Array.Accelerate as A hiding (mod')
import Data.Array.Accelerate.LLVM.PTX as GPU
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.Linear as L
import Data.Array.Accelerate.Data.Maybe as M
import Data.Array.Accelerate.Data.Bits as B
import Data.ReinterpretCast
import Criterion.Main

import Generics
import Implementation
import Variant
import qualified Data.Array.Accelerate.Interpreter as I
import Criterion.Types
import Data.Array.Accelerate.Unsafe (coerce)
import Data.Array.Accelerate.Sugar.Elt (Elt(tagsR))
import Data.Array.Accelerate.Representation.Tag (TagR(..))

-- used types
type Position     = V3 Float
type Direction    = V3 Float
type Normal       = (Position, Direction)
type Primitive    = CV [Sphere, Plane]

type Distance     = CV [Float, Float]

data Sphere = Sphere !Position !Float
  deriving (Show, Generic, Elt)

data Plane = Plane !Position !Direction
  deriving (Show, Generic, Elt)

data OtherPrim = PlaneP Plane | SphereP Sphere
  deriving (Show, Generic, Elt)

newtype EvenOtherPrim = EvenOther (Word8, Float, Float, Float, Float, Float, Float)
  deriving (Show, Generic)

mkPatterns [''Plane, ''Sphere, ''OtherPrim]

instance Element Distance Float where

pattern Hit :: Exp Float -> Exp Distance
pattern Hit value = Con0 value

pattern Miss :: Exp Distance
pattern Miss <- Con1 _
    where Miss = Con1 infinity

-- | nearest primitive that intersects with a ray
nearest :: Exp Position -> Exp Direction -> Collection [Sphere, Plane] -> Acc (Scalar Distance)
nearest position direction = A.fold Benchmark.min Miss . Implementation.map (Benchmark.intersect position direction)

-- | nearest primitive that intersects with a ray
nearest2 :: Exp Position -> Exp Direction -> Acc (Vector OtherPrim) -> Acc (Scalar Distance)
nearest2 position direction = A.fold Benchmark.min Miss . A.map (A.match $ intersect2 position direction)
  where intersect2 p d (SphereP_ sphere) = sphereIntersect p d sphere
        intersect2 p d (PlaneP_ plane)   = planeIntersect p d plane

-- | intersect object
intersect :: Exp Position -> Exp Direction -> Exp Primitive -> Exp Distance
intersect p d (Con0 sphere) = sphereIntersect p d sphere
intersect p d (Con1 plane)  = planeIntersect p d plane


inter = nearest (constant (L.V3 0 0 0)) (constant (L.V3 0 0 1))

inter2 = nearest2 (constant (L.V3 0 0 0)) (constant (L.V3 0 0 1))

-- | entry point for benchmarking
benchmark :: Benchmark -> P.IO ()
benchmark x = defaultMain [x]

benchCompact :: Benchmark
benchCompact = bgroup "examples"
    [   
        bench "small_element_int" (nf I.run (inter (elementW 128 128))),
        bench "small_sorted_int " (nf I.run (inter (sortedW 128 128))),
        bench "small_variant_int" (nf I.run (inter (variantW 128 128))),
        bench "large_element_int" (nf I.run (inter (elementW 20480 20480))),
        bench "large_sorted_int " (nf I.run (inter (sortedW 20480 20480))),
        bench "large_variant_int" (nf I.run (inter (variantW 20480 20480))),

        bench "small_element_cpu" (nf CPU.run (inter (elementW 128 128))),
        bench "small_sorted_cpu " (nf CPU.run (inter (sortedW 128 128))),
        bench "small_variant_cpu" (nf CPU.run (inter (variantW 128 128))),
        bench "large_element_cpu" (nf CPU.run (inter (elementW 20480 20480))),
        bench "large_sorted_cpu " (nf CPU.run (inter (sortedW 20480 20480))),
        bench "large_variant_cpu" (nf CPU.run (inter (variantW 20480 20480))),

        bench "small_element_gpu" (nf GPU.run (inter (elementW 128 128))),
        bench "small_sorted_gpu " (nf GPU.run (inter (sortedW 128 128))),
        bench "small_variant_gpu" (nf GPU.run (inter (variantW 128 128))),
        bench "large_element_gpu" (nf GPU.run (inter (elementW 20480 20480))),
        bench "large_sorted_gpu " (nf GPU.run (inter (sortedW 20480 20480))),
        bench "large_variant_gpu" (nf GPU.run (inter (variantW 20480 20480)))
    ]


benchUnCompact :: Benchmark
benchUnCompact = bgroup "examples"
    [   
        bench "small_element_int" (nf I.run (inter2 (elementWn 128 128))),
        bench "small_sorted_int " (nf I.run (inter2 (sortedWn 128 128))),
        bench "large_element_int" (nf I.run (inter2 (elementWn 20480 20480))),
        bench "large_sorted_int " (nf I.run (inter2 (sortedWn 20480 20480))),

        bench "small_element_cpu" (nf CPU.run (inter2 (elementWn 128 128))),
        bench "small_sorted_cpu " (nf CPU.run (inter2 (sortedWn 128 128))),
        bench "large_element_cpu" (nf CPU.run (inter2 (elementWn 20480 20480))),
        bench "large_sorted_cpu " (nf CPU.run (inter2 (sortedWn 20480 20480))),

        bench "small_element_gpu" (nf GPU.run (inter2 (elementWn 128 128))),
        bench "small_sorted_gpu " (nf GPU.run (inter2 (sortedWn 128 128))),
        bench "large_element_gpu" (nf GPU.run (inter2 (elementWn 20480 20480))),
        bench "large_sorted_gpu " (nf GPU.run (inter2 (sortedWn 20480 20480)))
    ]


benchCompactness :: Benchmark
benchCompactness  = bgroup "compactness"
    [   
        bench "yes-large_element_int" (nf I.run   (inter (elementW 204800 204800))),
        bench "yes-large_sorted_int " (nf I.run   (inter (sortedW 204800 204800))),
        bench "yes-large_element_cpu" (nf CPU.run (inter (elementW 204800 204800))),
        bench "yes-large_sorted_cpu " (nf CPU.run (inter (sortedW 204800 204800))),
        bench "yes-large_element_gpu" (nf GPU.run (inter (elementW 204800 204800))),
        bench "yes-large_sorted_gpu " (nf GPU.run (inter (sortedW 204800 204800))),

        bench "no-large_element_int" (nf I.run   (inter2 (elementWn 204800 204800))),
        bench "no-large_sorted_int " (nf I.run   (inter2 (sortedWn 204800 204800))),
        bench "no-large_element_cpu" (nf CPU.run (inter2 (elementWn 204800 204800))),
        bench "no-large_sorted_cpu " (nf CPU.run (inter2 (sortedWn 204800 204800))),
        bench "no-large_element_gpu" (nf GPU.run (inter2 (elementWn 204800 204800))),
        bench "no-large_sorted_gpu " (nf GPU.run (inter2 (sortedWn 204800 204800)))
    ]


-- input
l = inter (elementW 0 0)

spheres :: Int -> Array DIM1 Sphere
spheres = input rSphere

planes :: Int -> Array DIM1 Plane
planes = input rPlane

elementW :: Int -> Int -> Collection [Sphere, Plane]
elementW a b = ElementWise (use (input (\n -> rVariant n (if mod' n 2.0 P.<= 0.5 then 0 else 1)) (a + b)))

sortedW :: Int -> Int -> Collection [Sphere, Plane]
sortedW a b = SortedWise (use (input (\n -> rVariant n (if n P.<= P.fromIntegral a then 0 else 1)) (a + b)))

elementWn :: Int -> Int -> Acc (Vector OtherPrim)
elementWn a b = use (input (\n -> if mod' n 2.0 P.<= 0.5 then SphereP (rSphere n) else PlaneP (rPlane n)) (a + b))

sortedWn :: Int -> Int -> Acc (Vector OtherPrim)
sortedWn a b = use (input (\n -> if n P.<= P.fromIntegral a then SphereP (rSphere n) else PlaneP (rPlane n) ) (a + b))

variantW :: Int -> Int -> Collection [Sphere, Plane]
variantW a b = insert (use $ spheres a) (insert (use $ planes b) mempty)

input :: Elt a => (Float -> a) -> Int -> Array DIM1 a
input f n = fromList (Z :. n) (P.map f [0..(P.fromIntegral n)])

rSphere :: Float -> Sphere
rSphere n = Sphere (L.V3 (mod' (n * 0.1) 1.0) (mod' (n * 0.2) 1.0) (mod' (n * 0.3) 1.0)) (mod' (n * 0.4) 1.0)

rPlane :: Float -> Plane
rPlane n = Plane (L.V3 (mod' (n * 0.1) 1.0) (mod' (n * 0.2) 1.0) (mod' (n * 0.3) 1.0)) (L.V3 (mod' (n * 0.4) 1.0) (mod' (n * 0.5) 1.0) (mod' (n * 0.6) 1.0))

rVariant :: Float -> Word8 -> CV [Sphere, Plane]
rVariant n tag = VarUnion tag (((), f (n * 0.1)), (((), f (n * 0.2)), (((), f (n * 0.3)), (((), f (n * 0.4)), (((), f (n * 0.5)), f (n * 0.6))))))

f :: Float -> Word32
f n = floatToWord (mod' n 1.0)

xs :: Acc (Vector Float)
xs = use (fromList (Z:.10000) [0..] :: Vector Float)

ys :: Acc (Vector Float)
ys = use (fromList (Z:.10000) [1,3..] :: Vector Float)

-- indexing overhead

dotp :: Benchmark
dotp = bgroup "dot product"
    [
        bench "normal     " (whnf GPU.run (dotp1 xs ys)),
        bench "index      " (whnf GPU.run (dotp2 xs ys)),
        bench "integer    " (whnf GPU.run (dotp3 xs ys)),
        bench "reverse    " (whnf GPU.run (dotp4 xs (A.reverse ys))),
        bench "match      " (whnf GPU.run (dotp5 xs (A.map Just_ ys))),
        bench "conditional" (whnf GPU.run (dotp6 xs ys))
        --bench "mapping    " (whnf GPU.run (dotp7 (example xs ys)))
    ]

dotpcoerce :: Benchmark
dotpcoerce = bgroup "dot product coerce"
    [
        bench "normal     " (nf GPU.run (dotp1 xs ys)),
        bench "coerce     " (nf GPU.run (dotp2 xs ys))
    ]

dotpCoerce :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotpCoerce xs ys = A.fold (+) 0 (A.zipWith (\a b -> coerce (coerce a :: Exp Int32) * coerce (coerce b :: Exp Int32)) xs ys)



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


-- | branching
branch :: Benchmark
branch = bgroup "compare numbers"
    [
        bench "conditional  " (whnf GPU.run (A.zipWith (\a b -> cond   (a A.<= b) (a + 5) (a * 10)) ps ls)),
        bench "unconditional" (whnf GPU.run (A.zipWith (\a b -> uncond (a A.<= b) (a + 5) (a * 10)) ps ls))
    ]

ps :: Acc (Array DIM1 Int)
ps = use (fromList (Z :. 10000) [0..10000])

ls :: Acc (Array DIM1 Int)
ls = use (fromList (Z :. 10000) [10000,9999..0])


iso :: Exp Primitive -> Exp Sphere
iso (Con0 a) = a
iso (Con1 b) = constant (Sphere (L.V3 1 1 1) 9)

-- | generate randomly
objects :: Vector Sphere
objects = A.fromList (Z:.1) [Sphere (L.V3 0 0 1) 0.5]

-- | minimum distance
min :: Exp Distance -> Exp Distance -> Exp Distance
min (Union a) (Union b) = Hit (A.min a b)

-- | minimum distance
extract :: Exp Distance -> Exp Float
extract (Union a) = a

-- | sphere intersection
sphereIntersect :: Exp Position -> Exp Direction -> Exp Sphere -> Exp Distance
sphereIntersect origin direction (Sphere_ pos radius)
  = let
        p       = origin + ((pos - origin) `dot` direction) *^ direction
        d_cp    = norm (p - pos)
        sep     = p - origin
        miss    = d_cp >= radius || sep `dot` direction <= 0
    in miss ? (Miss, Hit (norm sep - sqrt (radius * radius - d_cp * d_cp)))

-- | plane intersection
planeIntersect :: Exp Position  -> Exp Direction -> Exp Plane -> Exp Distance
planeIntersect origin direction (Plane_ pos normal) = let theta = direction `dot` normal in
    theta >= 0 ? (Miss, Hit (((pos - origin) `dot` normal) / theta))

-- | maximum representable floating point value
infinity :: Exp Float
infinity = constant (P.encodeFloat m n)
  where
    a           = undefined :: Float
    b           = P.floatRadix a
    e           = P.floatDigits a
    (_, e')     = P.floatRange a
    m           = b P.^ e - 1
    n           = e' - e

