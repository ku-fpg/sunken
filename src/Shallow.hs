-- The shallow API
{-# LANGUAGE GADTs #-}
module Shallow
  (add)
  where

import           Types

add :: Int -> Int -> Int
add a b = a + b

