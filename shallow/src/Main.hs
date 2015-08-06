{-# LANGUAGE OverloadedStrings #-}
import           Graphics.Blank hiding (send, wait, eval)
import qualified Graphics.Blank as Blank

import           Web.KeyCode

import           Control.Monad.Reader
import           Control.Monad.State

import           Control.Concurrent

import           Control.Lens

import           Data.Maybe (fromMaybe)

import           Deep


buttons :: Lens' ([Bool], [Bool]) [Bool]
buttons = _1

leds :: Lens' ([Bool], [Bool]) [Bool]
leds = _2

send :: R () -> IO ()
send r = blankCanvas (3000 { events = ["keyup", "keydown"] })
    $ \context -> do
  Blank.send context initUI
  runReaderT (evalStateT r ([False, False, False, False], [False, False, False, False])) context
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

button :: Int -> R Bool
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


updateButtons :: R ()
updateButtons = do
  event <- lift $ lift . Blank.wait =<< ask
  case readButtonNum =<< keyCodeLookup <$> eWhich event of
      Just buttonNum ->
        buttons . ix buttonNum .= (eType event == "keydown")
      _              -> return ()

led :: Int -> Bool -> R ()
led ledNum state = do
  leds . ix ledNum .= state
  ledStates <- use leds
  runBlank $ clearCanvas >> drawLEDs ledStates
  where
    runBlank :: Canvas a -> R a
    runBlank c = do
      context <- ask
      liftIO $ Blank.send context c
{-# NOINLINE led #-}

loop :: R a -> R a
loop r = forever $ do
  updateButtons
  r
{-# NOINLINE loop #-}

wait :: Int -> R ()
wait ms =
  liftIO . threadDelay $ ms*10

main :: IO ()
main = do
  send $ do
    loop $ do
      b <- button 0
      led 0 b
      led 1 (not b)
      -- wait 100


-- Shallow->Deep transformation:

{-# RULES "ledE-intro" forall i b . led i b = ledE i (lit b) #-}

{-# RULES "lit-of-unLit" [~]
      forall x.
        lit (unLit x) = x
  #-}

{-# RULES "lower-button" [~]
      forall i f.
        button i >>= f
          =
        buttonE i >>= (\r -> f (unLit r))
  #-}


{-# RULES "eval-intro" [~]
      forall r.
        return (unLit r)
          =
        eval r
 #-}

{-# RULES "commute-not" [~]
      forall b.
        lit (not b)
          =
        notB (lit b)
  #-}

{-# RULES "lit-unLit" [~]
      forall b.
        lit (unLit b) = b
  #-}

