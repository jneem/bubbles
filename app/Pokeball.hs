module Main where

import Bubbles
import Numeric.LinearAlgebra.HMatrix
import Debug.Trace

-- Find the two points (in the span of x and y) that are fixed distances from two given points,
-- and then return the one that is further from `v`.
nextCenter :: Vec -> Double -> Vec -> Double -> Vec -> Vec
nextCenter x dx y dy v =
  let
    (w1, w2) = fixedDistances x dx y dy
  in
    if norm_2 (w1 - v) >= norm_2 (w2 - v)
    then w1
    else w2

sq :: Double -> Double
sq x = x * x

dbg :: Show a => a -> a
dbg x = traceShow x x

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- ks should have 8 elements
pokeball :: [Double] -> VoronoiCluster
pokeball ks =
  let
    cs = map center (colinear (take 5 ks)) ++ [c5, c6, c7]
    k i j = (ks !! i) - (ks !! j)
    d i j = sqrt (1 + sq (k i j))
    c i = cs !! i
    next k i j ell = nextCenter (c i) (d i k) (c j) (d j k) (c ell)
    c5 = dbg (next 5 3 4 0)
    c6 = next 6 3 5 4
    c7 = next 7 4 5 3
  in
  zipWith (\c k -> Cell { center = c, pressure = k }) cs (map (\k -> k - mean ks) ks)

main :: IO ()
main =
  let
    cl = pokeball [0, 3, 3, 1, 1, 3, 5, 5]
    clGram01 = project (firstThree 8) (gram 0.1 cl)
    clGram011 = project (firstThree 8) (gram 0.11 cl)
    clGram012 = project (firstThree 8) (gram 0.12 cl)
    clGram0125 = project (firstThree 8) (gram 0.125 cl)
    clGram013 = project (firstThree 8) (gram 0.13 cl)
    clGram015 = project (firstThree 8) (gram 0.15 cl)
    clGram025 = project (firstThree 8) (gram 0.25 cl)
    clGram05 = project (firstThree 8) (gram 0.5 cl)
    clGram09 = project (firstThree 8) (gram 0.9 cl)
    clGram10 = project (firstThree 8) (gram 1.0 cl)
    dilateVec = normalize $ vector [0, 1, 0]
    clDilate05 = dilate (dilateVec * 0.5) cl
    clDilate10 = dilate dilateVec cl
    clDilate15 = dilate (dilateVec * 1.5) cl
  in do
    writeFile "pokeball.inc" (concatMap show $ cameraDir cl : toNamedPovCells cl)
    writeFile "pokeballGram01.inc" (concatMap show $ cameraDir cl : toNamedPovCells clGram01)
    writeFile "pokeballGram011.inc" (concatMap show $ cameraDir cl : toNamedPovCells clGram011)
    writeFile "pokeballGram012.inc" (concatMap show $ cameraDir cl : toNamedPovCells clGram012)
    writeFile "pokeballGram0125.inc" (concatMap show $ cameraDir cl : toNamedPovCells clGram0125)
    writeFile "pokeballGram013.inc" (concatMap show $ cameraDir cl : toNamedPovCells clGram013)
    writeFile "pokeballGram015.inc" (concatMap show $ cameraDir cl : toNamedPovCells clGram015)
    writeFile "pokeballGram025.inc" (concatMap show $ cameraDir cl : toNamedPovCells clGram025)
    writeFile "pokeballGram05.inc" (concatMap show $ cameraDir cl : toNamedPovCells clGram05)
    writeFile "pokeballGram09.inc" (concatMap show $ cameraDir cl : toNamedPovCells clGram09)
    writeFile "pokeballGram10.inc" (concatMap show $ cameraDir cl : toNamedPovCells clGram10)
    writeFile "pokeballDilate05.inc" (concatMap show $ cameraDir cl : toNamedPovCells clDilate05)
    writeFile "pokeballDilate10.inc" (concatMap show $ cameraDir cl: toNamedPovCells clDilate10)
    writeFile "pokeballDilate15.inc" (concatMap show $ cameraDir cl: toNamedPovCells clDilate15)
