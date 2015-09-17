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

import           Control.Monad.Trans

button :: Int -> R Bool
button = fmap evalE . buttonE  -- Evaluate locally, for the shallow API
{-# NOINLINE button #-}

led :: Int -> Bool -> R ()
led ledNum = ledE ledNum . LitB
{-# NOINLINE led #-}

wait :: Int -> R ()
wait = lift . Action . Wait
{-# NOINLINE wait #-}

