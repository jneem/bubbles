{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bubbles

main :: IO ()
main =
  let
    cl = standard [0, 2, 3, 4]
  in
  putStrLn $ concatMap show $ cameraDir cl : toNamedPovCells cl
