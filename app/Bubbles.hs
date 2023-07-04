{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bubbles (Cell, VoronoiCluster, toNamedPovCells, Pov, standard, colinear) where

import GHC.TypeNats (KnownNat)
import Numeric.LinearAlgebra.Data ((!))
import Numeric.LinearAlgebra.Static
import Text.Printf
import qualified Numeric.LinearAlgebra.Static as LA
import Numeric.LinearAlgebra (toList, normalize)
import Data.List ()
import Data.List.HT (removeEach)

data Cell n = Cell
  { center :: R n,
    pressure :: Double
  }
  deriving (Show)

type VoronoiCluster n = [Cell n]

const4 :: Double -> R 4
const4 x = vec4 x x x x

const3 :: Double -> R 3
const3 x = vec3 x x x

proj3 :: L 3 4
proj3 =
  let m :: L 3 4 = snd (splitRows (eye - 0.25)) in
  let (_, _, m') = svdFlat m in
  tr m'

-- Make a standard bubble cluster with given pressures
standard :: R 4 -> VoronoiCluster 3
standard pressures =
  let ks = pressures - const4 (mean pressures) in
  let proj = eye - 0.25 in
  let m = sqrtm (eye / 2 + ks `outer` ks) in
  let m' = proj3 LA.<> m LA.<> proj in
  let mkCell c k = Cell { center = c, pressure = k } in
  let pressuresList = toList $ extract ks in
  zipWith mkCell (toColumns m') pressuresList

colinear :: R 4 -> VoronoiCluster 3
colinear pressures =
  let
    ks = pressures - const4 (mean pressures)
    ksList = toList (extract ks)
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
    centers = vec3 0 0 0 : zipWith
      (\r theta -> vec3 (r * cos theta) 0 (r * sin theta))
      centerDists
      angle
  in
  -- TODO: check whether the (desired) non-adjacent pairs are far enough apart to
  -- actually be non-adjacent.
  zipWith (\c p -> Cell { center = c, pressure = p }) centers ksList

-- A halfspace is a set of the form {x: <x, n> < t}, where "n" is the "normal"
-- and "t" is the "threshold."
data HalfSpace n = HalfSpace
  { normal :: R n,
    threshold :: Double
  }

type Convex n = [HalfSpace n]

newtype SphericalCell = SphericalCell (Convex 3)

cellToConvex :: forall n. KnownNat n => Cell n -> [Cell n] -> Convex n
cellToConvex cell =
  map halfspace
  where
    halfspace other =
      HalfSpace
        { normal = center cell - center other,
          threshold = pressure other - pressure cell
        }

convexes :: forall n. KnownNat n => VoronoiCluster n -> [Convex n]
convexes vc = map (uncurry cellToConvex) (removeEach vc)
 
data Pov
  = Line String
  | Block String [Pov]
  | Decl String Pov

class ToPov a where
  toPov :: a -> Pov

instance ToPov (HalfSpace 3) where
  toPov HalfSpace {normal = n', threshold = t'} =
    Line (printf "plane { <%.3f, %.3f, %.3f>, %.3f }" (n ! 0) (n ! 1) (n ! 2) t)
    where
      n = normalize $ unwrap n'
      t = t' / norm_2 n'

instance ToPov (Convex 3) where
  toPov hs = Block "intersection" $ map toPov hs

instance ToPov SphericalCell where
  toPov (SphericalCell cvx) =
    let unitSphere = Line "sphere { <0, 0, 0>, 1 }" in
    Block "intersection" [ unitSphere, toPov cvx ]

toPovCells :: VoronoiCluster 3 -> [Pov]
toPovCells vc = map (toPov . SphericalCell) (convexes vc)

toNamedPovCells :: VoronoiCluster 3 -> [Pov]
toNamedPovCells vc =
  let
    cells = toPovCells vc
    cellDecls = mapWithIndex (\idx cell -> Decl ("cl" ++ show idx) cell) cells
    dir' = sum $ map (\c -> center c * const3 (pressure c)) vc
    dir = if norm_2 dir' < 0.01
      then extract $ vec3 0 0 1
      else normalize $ extract dir'
    dirDecl = Decl "dir" (Line (printf "<%.3f, %.3f, %.3f>" (dir ! 0) (dir ! 1) (dir ! 2)))
  in
    dirDecl : cellDecls
  where
    mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
    mapWithIndex f =
      let
        mapFrom _ _ [] = []
        mapFrom idx g (x:xs) = g idx x : mapFrom (idx + 1) g xs
      in
        mapFrom 0 f
      

instance Show Pov where
  show = showR 0
    where
    showR :: Int -> Pov -> String
    showR indent (Line s) = replicate indent ' ' ++ s ++ "\n"
    showR indent (Block name sub) = replicate indent ' ' ++ name ++ " {\n"
      ++ concatMap (showR (indent + 4)) sub
      ++ replicate indent ' ' ++ "}\n"
    showR indent (Decl name val) = replicate indent ' ' ++ "#declare " ++ name ++ " =\n" ++ showR indent val ++ ";\n"
