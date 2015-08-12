{-# LANGUAGE GADTs #-}
module Shallow
  where

import           Types

shallowEval :: E a -> a
shallowEval (Not  b) = not (shallowEval b)
shallowEval (LitB b) = b

button :: Int -> R Bool
button = fmap shallowEval . Action . ButtonE

led :: Int -> Bool -> R ()
led ledNum = Action . LedE ledNum . LitB

loop :: R () -> R ()
loop = Loop

wait :: Int -> R ()
wait = Action . WaitE

