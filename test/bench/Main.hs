module Main (main) where

import Data.Function (($))
import System.IO (IO)

import Criterion.Main (defaultMain, bench, bgroup, whnf)

import KeyStats (fib)


main :: IO ()
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "9"  $ whnf fib 9
               , bench "11" $ whnf fib 11
               ]
  ]
