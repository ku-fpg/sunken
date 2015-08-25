{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Send
  (send)
  where

import           Graphics.Blank hiding (eval, send)
import qualified Graphics.Blank as Blank

import           Web.KeyCode

import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Control.Concurrent

import           Control.Lens

import           Data.Maybe (fromMaybe)
import           Types
import           Eval
import           Deep

data ButtonState
  = Up
  | Down
  | Press
  deriving (Eq)

data ServerState
  = ServerState
    { _buttons :: (ButtonState, ButtonState, ButtonState, ButtonState)
    , _leds    :: (Bool, Bool, Bool, Bool)
    }

$(makeLenses ''ServerState)

type Remote = StateT ServerState (ReaderT DeviceContext IO)

runCommand :: Action a -> Remote a
runCommand (Button i)        = LitB <$> buttonR i
-- Tracing would go on this line:
runCommand (Led i b@(Not _)) = ledR i (evalE b)
runCommand (Led i b)         = ledR i (evalE b)
runCommand (Wait ms )        = waitR ms

evalRemote :: E a -> Remote a
evalRemote = sendR . Action . LitA

send :: R () -> IO ()
send r = blankCanvas (3000 { events = ["keyup", "keypress", "keydown"] })
    $ \context -> do
  Blank.send context initUI
  runReaderT (evalStateT (sendR r) defaultServerState) context
  where
    defaultServerState
      = ServerState (Up, Up, Up, Up) (False, False, False, False)


sendR :: R a -> Remote a
sendR (Return x) = return x
sendR (Bind m f) = updateButtons >> sendR m >>= sendR . f
sendR (Action a) = updateButtons >> runCommand a
sendR (Loop m  ) = forever (updateButtons >> sendR m)
sendR (If b t f)
  | evalE b      = do
    liftIO $ putStrLn "Evaluating if on server (true branch)..."
    sendR t
  | otherwise    = do
    liftIO $ putStrLn "Evaluating if on server (false branch)..."
    sendR f

initUI :: Canvas ()
initUI = do
  drawLEDs (False, False, False, False)
  stroke ()

drawLEDs :: (Bool, Bool, Bool, Bool) -> Canvas ()
drawLEDs (a, b, c, d) = do
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

buttonR :: Int -> Remote Bool
buttonR buttonNum = do
  updateButtons
  bs <- use buttons
  let buttonState
        = fromMaybe (error ("Invalid button number: " ++ show buttonNum))
            $ bs ^? ix buttonNum
  return $ buttonState == Down

readButtonNum :: Key -> Maybe Int
readButtonNum KeyH = Just 0
readButtonNum KeyJ = Just 1
readButtonNum KeyK = Just 2
readButtonNum KeyL = Just 3
readButtonNum _    = Nothing


updateButtons :: Remote ()
updateButtons = do
  context <- ask

  events <- liftIO $ Blank.flush $ context
  updatePresses

  mapM_ updateWithEvent events
  where
    updateWithEvent :: Event -> Remote ()
    updateWithEvent event = do
      context <- ask
      liftIO $ Blank.send context sync

      case readButtonNum =<< keyCodeLookup <$> eWhich event of
          Just buttonNum ->
            buttons . ix buttonNum .= interpretEvent (eType event)
          _              -> return ()

    interpretEvent "keydown"  = Down
    interpretEvent "keypress" = Press
    interpretEvent "keyup"    = Up

updatePresses :: Remote ()
updatePresses = do
  buttons . partsOf each %= map update
  where
    update Press = Down
    update x     = x

ledR :: Int -> Bool -> Remote ()
ledR ledNum ledState = do
  leds . ix ledNum .= ledState
  ledStates <- use leds
  runBlank $ clearCanvas >> drawLEDs ledStates
  where
    runBlank :: Canvas a -> Remote a
    runBlank c = do
      context <- ask
      liftIO $ Blank.send context c

waitR :: Int -> Remote ()
waitR ms = liftIO . threadDelay $ ms*10

