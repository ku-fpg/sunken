{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Server
  (send)
  where
import           Graphics.Blank hiding (send, wait, eval)
import qualified Graphics.Blank as Blank

import           Web.KeyCode

import           Control.Monad.Reader
import           Control.Monad.State

import           Control.Concurrent

import           Control.Lens

import           Data.Maybe (fromMaybe)

import           Types
import           Deep

type ServerState = ([Bool], [Bool])

type Remote = StateT ServerState (ReaderT DeviceContext IO)

interpret :: R a -> Remote a
interpret (Return x) = return x
interpret (Bind m f) = interpret m >>= interpret . f
interpret (Action a) = runCommand a
interpret (Loop m  ) = forever (interpret m)
interpret (IfE eb t f) = do
  b <- interpret (Return eb)
  if evalE b   -- XXX: Is this the right way to do it?
    then interpret t
    else interpret f

evalE :: E a -> a
evalE (LitI i) = i
evalE (LitB b) = b
evalE (Not e)  = not (evalE e)

runCommand :: ActionE a -> Remote a
runCommand (ButtonE i) = LitB <$> button i
runCommand (LedE i b ) = led i (evalE b)
runCommand (WaitE ms ) = wait ms

buttons :: Lens' ([Bool], [Bool]) [Bool]
buttons = _1

leds :: Lens' ([Bool], [Bool]) [Bool]
leds = _2

send :: R () -> IO ()
send r = blankCanvas (3000 { events = ["keyup", "keydown"] })
    $ \context -> do
  Blank.send context initUI
  runReaderT (evalStateT (interpret r) ([False, False, False, False], [False, False, False, False])) context
{-# NOINLINE send #-}

initUI :: Canvas ()
initUI = do
  drawLEDs [False, False, False, False]
  -- drawButtons
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

-- drawButtons :: Canvas ()
-- drawButtons = do
--   strokeRect (60,300,100,50)
--   strokeRect (60 + 120,300,100,50)
--   strokeRect (60*2 + 180,300,100,50)
--   strokeRect (60*3 + 240,300,100,50)

button :: Int -> Remote Bool
button buttonNum = do
  bs <- use buttons
  return $ fromMaybe False $ bs ^? ix buttonNum
{-# NOINLINE button #-}

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
led ledNum state = do
  leds . ix ledNum .= state
  ledStates <- use leds
  runBlank $ clearCanvas >> drawLEDs ledStates
  where
    runBlank :: Canvas a -> Remote a
    runBlank c = do
      context <- ask
      liftIO $ Blank.send context c
{-# NOINLINE led #-}

loop :: Remote a -> Remote a
loop r = forever $ do
  updateButtons
  r
{-# NOINLINE loop #-}

wait :: Int -> Remote ()
wait ms =
  liftIO . threadDelay $ ms*10
