-- The shallow API
{-# LANGUAGE GADTs #-}
module Shallow
  (button
  ,led
  ,loop
  ,wait
  )
  where

import           Types
import           Deep
import           Eval

button :: Int -> R Bool
button = fmap eval . buttonE  -- Evaluate locally, for the shallow API

led :: Int -> Bool -> R ()
led ledNum = ledE ledNum . LitB

wait :: Int -> R ()
wait = Action . Wait

