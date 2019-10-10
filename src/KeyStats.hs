module KeyStats
    ( main
    , fib
    )
  where

import Prelude (Integer, (+), (-), (*), (^), abs)

import Control.Concurrent (MVar, newMVar)
import Control.Exception (bracket)
import Control.Monad ((>>=), (>>), forever, mapM_)
import Data.Bits ((.|.))
import Data.Bool (otherwise)
import Data.Function (($))
import Data.Ord ((<))
import System.IO (IO, print)

import Graphics.X11.Xlib
    ( Display
    , EventMask
    , Window
    , allocaXEvent
    , asKeyEvent
    , closeDisplay
    , defaultRootWindow
    , keyPressMask
    , lookupString
    , nextEvent
    , openDisplay
    , pointerMotionMask
    , selectInput
    )
import Graphics.X11.Xlib.Extras (getEvent, queryTree)
import qualified Data.Map.Strict as M


fib :: Integer -> Integer
fib m | m < 0     = (-1 ^ abs m) * fib (abs m)
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

selectTree :: Display -> Window -> EventMask -> IO ()
selectTree d w m = do
    selectInput d w m
    (_,_,ws) <- queryTree d w
    mapM_ (\x -> selectTree d x m) ws

main :: IO ()
main = do
    bracket (openDisplay "") closeDisplay $ \ display -> do
        print display
        let root = defaultRootWindow display
        print root
        selectTree display root keyPressMask -- $ keyPressMask .|. buttonReleaseMask
        selectTree display root $ keyPressMask .|. pointerMotionMask
        allocaXEvent $ \ e -> do
            forever $ do
                nextEvent display e
                getEvent e >>= print
                lookupString (asKeyEvent e) >>= print
