-- The shallow API
{-# LANGUAGE GADTs #-}
module Shallow
  (add)
  where

import           Types

add :: Int -> Int -> E Int
add a b = Lit (a + b)

