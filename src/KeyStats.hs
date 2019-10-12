{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module KeyStats (main) where

import Prelude (Integer, succ)

import Control.Applicative (pure)
import Control.Concurrent (forkIO, modifyMVar_, newMVar, swapMVar, threadDelay)
import Control.Exception (bracket)
import Control.Monad ((>>=), forever, when)
import Data.Bits ((.|.))
import Data.Bool (Bool(False))
import Data.Eq ((==))
import Data.Foldable (for_)
import Data.Function (($), (.))
import System.IO (IO, print)

import Graphics.X11.Xlib
    ( Display
    , EventMask
    , Window
    , allocaXEvent
    , closeDisplay
    , createNotify
    , keyPress
    , defaultRootWindow
    , keyPressMask
    , nextEvent
    , openDisplay
    , selectInput
    , substructureNotifyMask
    , sync
    )
import Graphics.X11.Xlib.Extras
    ( Event(AnyEvent, KeyEvent)
    , ev_event_type
    , ev_window
    , getEvent
    , queryTree
    )

selectTree :: EventMask -> Display -> Window -> IO ()
selectTree eventMask display = go
  where
    go w = do
        (_root, _parent, children) <- queryTree display w
        selectInput display w eventMask
        for_ children go

setAllEvents :: Display -> Window -> IO ()
setAllEvents = selectTree (substructureNotifyMask .|. keyPressMask)

keyboardCollector :: IO () -> IO ()
keyboardCollector tick = withDisplay $ \ display -> do
    setAllEvents display (defaultRootWindow display)
    sync display False
    allocaXEvent $ \ e -> forever $ do
        nextEvent display e
        getEvent e >>= \case
            AnyEvent{ev_event_type, ev_window} ->
                when (ev_event_type == createNotify)
                $ setAllEvents display ev_window
            KeyEvent{ev_event_type} -> when (ev_event_type == keyPress) tick
            _ev -> pure ()
  where
    withDisplay = bracket (openDisplay "") closeDisplay

main :: IO ()
main = do
    keyboardCounter <- newMVar zero
    let keyboardTick = modifyMVar_ keyboardCounter (pure . succ)
    _ <- forkIO $ keyboardCollector keyboardTick
    forever $ do
        threadDelay 60000000
        x <- swapMVar keyboardCounter zero
        print x
  where
    zero :: Integer
    zero = 0
