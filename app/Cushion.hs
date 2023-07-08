{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bubbles
import Numeric.LinearAlgebra.Static

main :: IO ()
main =
  let cl =
        [ Cell {center = vec3 0 0 0, pressure = 0},
          Cell {center = vec3 1 1 0, pressure = 1},
          Cell {center = vec3 (-1) 1 0, pressure = 1},
          Cell {center = vec3 0.5 2 (sqrt 3 / 2), pressure = 2},
          Cell {center = vec3 (-0.5) 2 (sqrt 3 / 2), pressure = 2}
        ]
   in writeFile "degenerate.inc" (concatMap show $ cameraDir cl : toNamedPovCells cl)
