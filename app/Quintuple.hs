module Main where

import Bubbles
import Numeric.LinearAlgebra
import Text.Printf

randomProj :: Seed -> Matrix Double
randomProj s =
  let
    -- A random 4x3 matrix with Gaussian entries
    m = gaussianSample s 5 (vector [0, 0, 0]) (sym $ ident 3)
  in
    tr $ orth m

main :: IO ()
main =
  let
    cl0 = standard [0, 0, 0, 0, 0, 0]
    cl i = project (randomProj ((i - 1) * 10 + 1)) cl0
    write i =
        let
          name = printf "quintuple%02d.inc" i
          c = cl i
        in
        writeFile name (concatMap show $ cameraDir c : toNamedPovCells c)
  in do
    mapM_ write [1..10]
