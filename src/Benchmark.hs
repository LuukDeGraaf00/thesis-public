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
type Primitive    = CV [Sphere, Plane, Triangle]
type Distance     = CV [Float, Float]

data Sphere = Sphere !Position !Float
  deriving (Show, Generic, Elt)

data Plane = Plane !Position !Direction
  deriving (Show, Generic, Elt)

data Triangle = Triangle !Position !Position !Position
  deriving (Show, Generic, Elt)

data OtherPrim = PlaneP Plane | SphereP Sphere | TriangleP Triangle
  deriving (Show, Generic, Elt)

mkPatterns [''Plane, ''Sphere, ''Triangle, ''OtherPrim]

instance Element Distance Float where
instance Element Distance (Word8, Float)

pattern Hit :: Exp Float -> Exp Distance
pattern Hit value = Con0 value

pattern Result :: Exp Word8 -> Exp Float -> Exp Distance
pattern Result tag value = Union (T2 tag value)

pattern Miss :: Exp Distance
pattern Miss <- Con1 _
    where Miss = Con1 infinity

-- | nearest primitive that intersects with a ray
nearest :: Exp Position -> Exp Direction -> Collection [Sphere, Plane, Triangle] -> Acc (Scalar Distance)
nearest position direction = A.fold Benchmark.min Miss . Implementation.map (Benchmark.intersect position direction)

-- | intersect object
intersect :: Exp Position -> Exp Direction -> Exp Primitive -> Exp Distance
intersect p d (Con0 sphere)   = sphereIntersect p d sphere
intersect p d (Con1 plane)    = planeIntersect p d plane
intersect p d (Con2 triangle) = triangleIntersect p d triangle

-- | simplified code for benchmarking
interElement :: Acc (Vector Primitive) -> Acc (Scalar Distance)
interElement = A.fold Benchmark.min Miss . A.map (match $ Benchmark.intersect (constant (L.V3 0 0 0)) (constant (L.V3 0 0 1)))

-- | simplified code for benchmarking
interElementN :: Acc (Vector OtherPrim) -> Acc (Scalar Distance)
interElementN = A.fold Benchmark.min Miss . A.map (match $ intersect2 (constant (L.V3 0 0 0)) (constant (L.V3 0 0 1)))
  where intersect2 p d (SphereP_ sphere)     = sphereIntersect p d sphere
        intersect2 p d (PlaneP_ plane)       = planeIntersect p d plane
        intersect2 p d (TriangleP_ triangle) = triangleIntersect p d triangle

-- | simplified code for benchmarking
interVariant :: Acc (Vector Sphere, Vector Plane, Vector Triangle) -> Acc (Scalar Distance)
interVariant (T3 a b c) = A.zipWith3 (\x y z -> Benchmark.min x (Benchmark.min y z))
                (A.fold Benchmark.min Miss $ A.map (f1 . Construct) a)
                (A.fold Benchmark.min Miss $ A.map (f2 . Construct) b)
                (A.fold Benchmark.min Miss $ A.map (f3 . Construct) c)
    where (f1 : f2 : f3 : _) = functions (Benchmark.intersect (constant (L.V3 0 0 0)) (constant (L.V3 0 0 1)))

-- | simplified code for benchmarking
interVariant2 :: (Int, Int, Int) -> Acc (Vector Sphere, Vector Plane, Vector Triangle) -> Acc (Scalar Distance)
interVariant2 (x, y, z) (T3 a b c) = A.fold Benchmark.min Miss (A.generate (I1 (constant (x + y + z))) f)
  where f (I1 i) = i A.< constant x A.? (s (a A.!! i), i A.< constant (x + y) A.? (p (b A.!! (i - constant x)), t (c A.!! (i - constant (x + y)))))
        s        = sphereIntersect (constant (L.V3 0 0 0)) (constant (L.V3 0 0 1))
        p        = planeIntersect (constant (L.V3 0 0 0)) (constant (L.V3 0 0 1))
        t        = triangleIntersect (constant (L.V3 0 0 0)) (constant (L.V3 0 0 1))

-- | entry point for benchmarking
benchmark :: Benchmark -> P.IO ()
benchmark x = defaultMain [x]

b :: (Int, Int, Int) -> P.IO ()
b r = benchmark $ bgroup "Devices" [total r CPU.run1 "CPU", total r GPU.run1 "GPU"]

c :: Int -> P.IO ()
c r = benchmark $ bgroup "Devices" [conditional r CPU.run1, conditional r GPU.run1]


allDevice :: (Int, Int, Int) -> Benchmark
allDevice r = bgroup "Devices" [total r I.run1 "Interpreter", total r CPU.run1 "CPU", total r GPU.run1 "GPU"]

total :: forall. (Int, Int, Int) -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b) -> P.String -> Benchmark
total r@(a,b,c) f t = bgroup (t P.++ "/" P.++ P.show (a P.+ b P.+ c))
    [
        bench "Split/Variant"   (nf (f interVariant)      (variantW  r)),
        bench "Joined/Variant"  (nf (f (interVariant2 r)) (variantW  r)),
        bench "Naive/Element"   (nf (f interElementN)     (elementWn r)),
        bench "Naive/Sorted"    (nf (f interElementN)     (sortedWn  r)),
        bench "Compact/Element" (nf (f interElement)      (elementW  r)),
        bench "Compact/Sorted " (nf (f interElement)      (sortedW   r))
    ]


sphereExample :: Int -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b) -> Benchmark
sphereExample count f = bgroup "Sphere"
  [
      bench "Conditional  "  (nf (f cond)   (input rSphere count)),
      bench "Unconditional"  (nf (f uncond) (input rSphere count))
  ]
    where cond   = A.fold Benchmark.min Miss . A.map (sphereIntersect  (constant (L.V3 0 0 0)) (constant (L.V3 0 0 1)))
          uncond = A.fold Benchmark.min Miss . A.map (sphereIntersect2 (constant (L.V3 0 0 0)) (constant (L.V3 0 0 1)))


conditional :: Int -> (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b) -> Benchmark
conditional count f = bgroup "Masking"
  [
      bench "Conditional  "  (nf (f a)   (A.fromList (Z :. count) [0..(count - 1)])),
      bench "Unconditional"  (nf (f b) (A.fromList (Z :. count) [0..(count - 1)]))
  ]
    where a = A.map (\i -> i A.< 50 A.? (i * 5, i - 2))
          b = A.map (\i -> uncond (i A.< 50) (i * 5) (i - 2))



-- input
variantW :: (Int, Int, Int)-> (Vector Sphere, Vector Plane, Vector Triangle)
variantW (a,b,c) = (input rSphere a, input rPlane b, input rTriangle c)

elementW :: (Int, Int, Int)-> Vector Primitive
elementW (a,b,c) = input (\n -> rVariant n (if mod' n 2.0 P.<= 0.5 then 0 else 1)) (a + b + c)

sortedW :: (Int, Int, Int) -> Vector Primitive
sortedW (a,b,c) = input (\n -> rVariant n (if n P.<= P.fromIntegral a then 0 else 1)) (a + b + c)

elementWn :: (Int, Int, Int) -> Vector OtherPrim
elementWn (a,b,c) = input (\n -> if mod' n 3.0 P.<= 0.5 then SphereP (rSphere n) else (if mod' n 3.0 P.<= 1.5 then PlaneP (rPlane n) else TriangleP (rTriangle n))) (a + b + c)

sortedWn :: (Int, Int, Int) -> Vector OtherPrim
sortedWn (a,b,c) = input (\n -> if n P.<= P.fromIntegral a then SphereP (rSphere n) else (if n P.<= P.fromIntegral (a + b) then PlaneP (rPlane n) else TriangleP (rTriangle n))) (a + b + c)

-- utility
input :: Elt a => (Float -> a) -> Int -> Array DIM1 a
input f n = fromList (Z :. n) (P.map f [0..(P.fromIntegral n)])

rSphere :: Float -> Sphere
rSphere n = Sphere (L.V3 (mod' (n * 0.1) 1.0) (mod' (n * 0.2) 1.0) (mod' (n * 0.3) 1.0)) (mod' (n * 0.4) 1.0)

rPlane :: Float -> Plane
rPlane n = Plane (L.V3 (mod' (n * 0.1) 1.0) (mod' (n * 0.2) 1.0) (mod' (n * 0.3) 1.0)) (L.V3 (mod' (n * 0.4) 1.0) (mod' (n * 0.5) 1.0) (mod' (n * 0.6) 1.0))

rTriangle :: Float -> Triangle
rTriangle n = Triangle a b c
    where a = L.V3 (mod' (n * 0.1) 1.0) (mod' (n * 0.2) 1.0) (mod' (n * 0.3) 1.0)
          b = L.V3 (mod' (n * 0.4) 1.0) (mod' (n * 0.5) 1.0) (mod' (n * 0.6) 1.0)
          c = L.V3 (mod' (n * 0.7) 1.0) (mod' (n * 0.8) 1.0) (mod' (n * 0.9) 1.0)

rVariant :: Float -> Word8 -> Primitive
rVariant n tag = VarUnion tag (((), f (n * 0.1)),
    (((), f (n * 0.2)),
    (((), f (n * 0.3)),
    (((), f (n * 0.4)),
    (((), f (n * 0.5)),
    (((), f (n * 0.6)),
    (((), f (n * 0.7)),
    (((), f (n * 0.8)), f (n * 0.9)))))))))

f :: Float -> Word32
f n = floatToWord (mod' n 1.0)

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

-- | sphere intersection
sphereIntersect2 :: Exp Position -> Exp Direction -> Exp Sphere -> Exp Distance
sphereIntersect2 origin direction (Sphere_ pos radius)
  = let
        p       = origin + ((pos - origin) `dot` direction) *^ direction
        d_cp    = norm (p - pos)
        sep     = p - origin
        miss    = d_cp >= radius || sep `dot` direction <= 0
    in Result (coerce miss) (norm sep - sqrt (radius * radius - d_cp * d_cp))

-- | plane intersection
planeIntersect :: Exp Position -> Exp Direction -> Exp Plane -> Exp Distance
planeIntersect origin direction (Plane_ pos normal) = let theta = direction `dot` normal in
    theta >= 0 ? (Miss, Hit (((pos - origin) `dot` normal) / theta))

-- | triangle intersection
triangleIntersect :: Exp Position -> Exp Direction -> Exp Triangle -> Exp Distance
triangleIntersect p d (Triangle_ a b c) = -0.001 < r && r < 0.0001 ? (Miss,
                                          u < 0.0 || u > 1.0       ? (Miss,
                                          v < 0.0 || u + v > 1.0   ? (Miss,
                                          distance < 0.0           ? (Miss, Hit distance))))
    where (e1, e2)  = (b - a, c - a)
          (h, r)    = (cross d e2, dot e1 h)
          f         = 1.0 / r
          (s, u)    = (p - a, f * dot s h)
          (q, v)    = (cross s e1, f * dot d q)
          distance  = f * dot e2 q


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

