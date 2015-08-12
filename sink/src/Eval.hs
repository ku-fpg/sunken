{-# LANGUAGE GADTs #-}
module Eval
  (evalE)
  where

import           Types

evalE :: E a -> a
evalE (LitB b) = b
evalE (Not  b) = not (evalE b)

