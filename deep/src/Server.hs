{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}

module Server (Remote, evalE, runServer, runCommand) where

import           Graphics.Blank hiding (send, wait)
import qualified Graphics.Blank as Blank

import           Web.KeyCode

import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Control.Concurrent

import           Control.Lens

import           Data.Maybe (fromMaybe)

import           Types

data ServerState
  = ServerState
    { _buttons :: [Bool]
    , _leds    :: [Bool]
    }

$(makeLenses ''ServerState)

type Remote = StateT ServerState (ReaderT DeviceContext IO)

runCommand :: Action a -> Remote a
runCommand (Button i) = LitB <$> button i
runCommand (Led i b ) = led i (evalE b)
runCommand (Wait ms ) = wait ms
runCommand (WaitE ms) = wait (evalE ms)

evalE :: E a -> a
evalE (LitI i) = i
evalE (LitB b) = b
evalE (Not e)  = not (evalE e)

runServer :: Remote () -> IO ()
runServer r = blankCanvas (3000 { events = ["keyup", "keydown"] })
    $ \context -> do
  Blank.send context initUI
  runReaderT (evalStateT r defaultServerState) context
  where
    defaultServerState
      = ServerState [False, False, False, False] [False, False, False, False]

initUI :: Canvas ()
initUI = do
  drawLEDs [False, False, False, False]
  stroke ()

drawLEDs :: [Bool] -> Canvas ()
drawLEDs [a, b, c, d] = do
  beginPath ()
  arc (100+offset, 75, 20, 0, 2*pi, False)
  closePath ()
  when a $ fill ()
  stroke()

  beginPath ()
  moveTo (220+offset, 75)
  arc (200+offset, 75, 20, 0, 2*pi, False)
  closePath ()
  when b $ fill ()
  stroke()

  beginPath ()
  moveTo (320+offset, 75)
  arc (300+offset, 75, 20, 0, 2*pi, False)
  closePath ()
  when c $ fill ()
  stroke()

  beginPath()
  moveTo (420+offset, 75)
  arc (400+offset, 75, 20, 0, 2*pi, False)
  closePath()
  when d $ fill ()
  stroke()
  where
    offset = 35
drawLEDs _ = error "drawLeds: You must give a list of exactly 4 LEDs"

button :: Int -> Remote Bool
button buttonNum = do
  updateButtons
  bs <- use buttons
  return $ fromMaybe False $ bs ^? ix buttonNum

readButtonNum :: Key -> Maybe Int
readButtonNum KeyH = Just 0
readButtonNum KeyJ = Just 1
readButtonNum KeyK = Just 2
readButtonNum KeyL = Just 3
readButtonNum _    = Nothing


updateButtons :: Remote ()
updateButtons = do
  event <- lift $ lift . Blank.wait =<< ask
  case readButtonNum =<< keyCodeLookup <$> eWhich event of
      Just buttonNum ->
        buttons . ix buttonNum .= (eType event == "keydown")
      _              -> return ()

led :: Int -> Bool -> Remote ()
led ledNum ledState = do
  leds . ix ledNum .= ledState
  ledStates <- use leds
  runBlank $ clearCanvas >> drawLEDs ledStates
  where
    runBlank :: Canvas a -> Remote a
    runBlank c = do
      context <- ask
      liftIO $ Blank.send context c

wait :: Int -> Remote ()
wait ms = liftIO . threadDelay $ ms*10

