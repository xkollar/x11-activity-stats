module KeyStats
    ( main
    , fib
    )
  where

import Prelude (Integer, (+), (-), (*), (^), abs)

import Data.Bool (otherwise)
import Data.Ord ((<))

import System.IO (IO, print)


fib :: Integer -> Integer
fib m | m < 0     = (-1 ^ abs m) * fib (abs m)
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

main :: IO ()
main = print ()
