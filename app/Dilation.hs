{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bubbles
import System.Environment
import Text.Printf
import Numeric.LinearAlgebra

main :: IO ()
main = do
  args <- getArgs
  let [p0, p1, p2, p3] = map read (take 4 args)
  let base = args !! 4
  let cl = colinear [p0, p1, p2, p3]
  let thetas = [vector [0, t / 50, 0] | t <- [0..150]]
  let filenames :: [String] = [printf "%s%03d.inc" base i | i <- [0..150] :: [Int]]
  let dir = cameraDir cl
  -- TODO: fix the dir variable from the first file.
  let write (name, theta) =
        writeFile name (concatMap show $ dir : toNamedPovCells (dilate theta cl))
  mapM_ write (zip filenames thetas)
