{-# LANGUAGE GADTs #-}
module Eval
  (evalE)
  where

import           Types

evalE :: E a -> a
evalE (Add a b) = evalE a + evalE b
evalE (Lit a)   = a

