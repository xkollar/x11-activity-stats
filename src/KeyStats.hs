{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module KeyStats
    ( main
    , fib
    )
  where

import Prelude (Integer, (+), (-), (*), (^), abs)

import Control.Applicative (pure)
import Control.Concurrent (MVar, newMVar)
import Control.Exception (bracket)
import Control.Monad ((>>=), (>>), forever, when)
import Data.Bits ((.|.))
import Data.Bool (Bool(False), otherwise)
import Data.Eq ((==))
import Data.Foldable (for_)
import Data.Function (($), (.))
import Data.Ord ((<))
import System.IO (IO, print, putStr, putStrLn)

import Graphics.X11.Xlib
    ( Display
    , EventMask
    , Window
    , allocaXEvent
    , asKeyEvent
    , closeDisplay
    , createNotify
    , keyPress
    , defaultRootWindow
    , enterWindowMask
    , keyPressMask
    , lookupString
    , nextEvent
    , openDisplay
    , pointerMotionMask
    , propertyChangeMask
    , selectInput
    , substructureNotifyMask
    , sync
    )
import Graphics.X11.Xlib.Extras
    ( Event(AnyEvent, KeyEvent)
    , ev_event_type
    , ev_window
    , getEvent
    , none
    , queryTree
    )
import qualified Data.Map.Strict as M


fib :: Integer -> Integer
fib m | m < 0     = (-1 ^ abs m) * fib (abs m)
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

selectTree :: EventMask -> Display -> Window -> IO ()
selectTree eventMask display = go
  where
    go w = do
        (_root, _parent, children) <- queryTree display w
        selectInput display w eventMask
        for_ children go

setAllEvents :: Display -> Window -> IO ()
setAllEvents = selectTree (substructureNotifyMask .|. keyPressMask)

main :: IO ()
main = do
    bracket (openDisplay "") closeDisplay $ \ display -> do
        print display
        setAllEvents display (defaultRootWindow display)
        sync display False
        allocaXEvent $ \ e -> forever $ do
            nextEvent display e
            getEvent e >>= \case
                ev@AnyEvent{ev_event_type, ev_window} -> do
                    putStr "Any: " >> print ev
                    when (ev_event_type == createNotify) $ setAllEvents display ev_window
                KeyEvent{ev_event_type} -> do
                    when (ev_event_type == keyPress) $ do lookupString (asKeyEvent e) >>= print
                ev -> pure ()
