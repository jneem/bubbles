{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bubbles
import Numeric.LinearAlgebra.Static

main :: IO ()
main =
  let
    cl = colinear $ vec4 0 2 3 4
  in
  putStrLn $ concatMap show $ toNamedPovCells cl
