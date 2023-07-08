{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}

module Bubbles (Cell (..), VoronoiCluster, toNamedPovCells, Pov, standard, colinear, dilate, cameraDir,
) where

import Text.Printf
import Data.List ()
import Data.List.HT (removeEach)
import Numeric.LinearAlgebra.HMatrix

type Vec = Vector Double
data Cell = Cell
  { center :: Vec,
    pressure :: Double
  }
  deriving (Show)

type VoronoiCluster = [Cell]

pressures :: VoronoiCluster -> Vec
pressures cl = vector $ map pressure cl

centers :: VoronoiCluster -> Matrix Double
centers cl = fromRows $ map center cl

dimension :: VoronoiCluster -> Int
dimension cl = size $ center $ head cl

-- Multiply a vector by a scalar
times :: Vec -> Double -> Vec
times v x = v * konst x (size v)

-- An isometry from E_n to R_n, as a (n+1) \times n matrix
projE :: Int -> Matrix Double
projE n =
  let
    a = dropColumns 1 (ident (n + 1) - 1 / (fromIntegral n + 1)) 
    (u, _, _) = thinSVD a
  in
  u

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- Make a standard bubble cluster with given pressures
standard :: [Double] -> VoronoiCluster
standard pressures =
  let
    n = length pressures
    ks = vector pressures - scalar (mean pressures)
    proj = projE 3
    ks' = proj #> ks
    m = sqrtm (ident n / 2 + ks' `outer` ks')
    m' = tr proj `mul` m
    mkCell c k = Cell { center = c, pressure = k }
    pressuresList = toList ks
  in
  zipWith mkCell (toRows m') pressuresList

colinear :: [Double] -> VoronoiCluster
colinear pressures =
  let
    ks = vector pressures - scalar (mean pressures)
    ksList = toList ks
    -- We take the first center at the origin, and then for each cell, what is
    -- its distance to the center?
    centerDists = map (\k -> sqrt (1 + (k - head ksList)^(2::Int))) (tail ksList)
    -- For all but the first two centers, what is the distance to the previous one?
    prevDists = zipWith (\ki kj -> sqrt (1 + (ki - kj)^(2::Int))) (tail ksList) (tail $ tail ksList)
    -- For all but the first two centers, the angle between each center and the previous one.
    prevAngle = zipWith3
      (\a b c -> acos ((a^(2::Int) + b^(2::Int) - c^(2::Int)) / (2.0*a*b)))
      centerDists
      (tail centerDists)
      prevDists
    angle = scanl (+) 0 prevAngle
    centers = vector [0, 0, 0] : zipWith
      (\r theta -> vector [r * cos theta, 0, r * sin theta])
      centerDists
      angle
  in
  -- TODO: check whether the (desired) non-adjacent pairs are far enough apart to
  -- actually be non-adjacent.
  zipWith (\c p -> Cell { center = c, pressure = p }) centers ksList

-- FIXME: This assumes the input cluster has n+1 cells in dimension n. It would be nice to allow
-- m <= n + 1 cells...
-- gram :: forall n. (KnownNat n, KnownNat (n+1), n <= n+1) => Double -> VoronoiCluster n -> VoronoiCluster n
-- gram t cl = error "todo"
--   where
--     ks :: R (n + 1) = pressures cl
--     nNat = natVal (center $ head cl)
--     eyeE :: L (n + 1) (n + 1) = eye - konst (1 / fromIntegral nNat)
--     targetGram = eyeE / 2 + ks `outer` ks
--     projE :: L n n = eye - 0.25

-- Apply the "dilation" Mobius transform in the direction `theta`
dilate :: Vec -> VoronoiCluster -> VoronoiCluster
dilate theta cl =
  let
    t = norm_2 theta
    cthetas = [dot theta (center c) / t | c <- cl]
    ks = [pressure c | c <- cl]
    new_ks = [cosh t * k - sinh t * ctheta | (k, ctheta) <- zip ks cthetas]
    new_cthetas = [cosh t * ctheta - sinh t * k | (k, ctheta) <- zip ks cthetas]
    new_cs = [center cell + theta `times` ((new_ctheta - ctheta) / t) | (cell, ctheta, new_ctheta) <- zip3 cl cthetas new_cthetas]
  in
    if t < 0.0001
    then cl
    else [Cell { center = c, pressure = p } | (c, p) <- zip new_cs new_ks]

-- A halfspace is a set of the form {x: <x, n> < t}, where "n" is the "normal"
-- and "t" is the "threshold."
data HalfSpace = HalfSpace
  { normal :: Vec,
    threshold :: Double
  }

type Convex = [HalfSpace]

newtype SphericalCell = SphericalCell Convex

cellToConvex :: Cell -> [Cell] -> Convex
cellToConvex cell =
  map halfspace
  where
    halfspace other =
      HalfSpace
        { normal = center cell - center other,
          threshold = pressure other - pressure cell
        }

convexes :: VoronoiCluster -> [Convex]
convexes vc = map (uncurry cellToConvex) (removeEach vc)
 
data Pov
  = Line String
  | Block String [Pov]
  | Decl String Pov

class ToPov a where
  toPov :: a -> Pov

instance ToPov HalfSpace where
  toPov HalfSpace {normal = n', threshold = t'} =
    Line (printf "plane { <%.3f, %.3f, %.3f>, %.3f }" (n ! 0) (n ! 1) (n ! 2) t)
    where
      n = normalize n'
      t = t' / norm_2 n'

instance ToPov Convex where
  toPov hs = Block "intersection" $ map toPov hs

instance ToPov SphericalCell where
  toPov (SphericalCell cvx) =
    let unitSphere = Line "sphere { <0, 0, 0>, 1 }" in
    Block "intersection" [ unitSphere, toPov cvx ]

toPovCells :: VoronoiCluster -> [Pov]
toPovCells vc = map (toPov . SphericalCell) (convexes vc)

toNamedPovCells :: VoronoiCluster -> [Pov]
toNamedPovCells vc =
  let
    cells = toPovCells vc
    cellDecls = mapWithIndex (\idx cell -> Decl ("cl" ++ show idx) cell) cells
  in
    cellDecls
  where
    mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
    mapWithIndex f =
      let
        mapFrom _ _ [] = []
        mapFrom idx g (x:xs) = g idx x : mapFrom (idx + 1) g xs
      in
        mapFrom 0 f

cameraDir :: VoronoiCluster -> Pov
cameraDir vc =
  let
    dir' = sum $ map (\c -> center c * scalar (pressure c)) vc
    dir = if norm_2 dir' < 0.01
      then vector [0, 0, 1]
      else dir'
  in
    Decl "dir" (Line (printf "<%.3f, %.3f, %.3f>" (dir ! 0) (dir ! 1) (dir ! 2)))

instance Show Pov where
  show = showR 0
    where
    showR :: Int -> Pov -> String
    showR indent (Line s) = replicate indent ' ' ++ s ++ "\n"
    showR indent (Block name sub) = replicate indent ' ' ++ name ++ " {\n"
      ++ concatMap (showR (indent + 4)) sub
      ++ replicate indent ' ' ++ "}\n"
    showR indent (Decl name val) = replicate indent ' ' ++ "#declare " ++ name ++ " =\n" ++ showR indent val ++ ";\n"
