{-# LANGUAGE GADTs #-}
module Eval
  (eval)
  where

import           Types

eval :: E a -> a
eval (LitB b) = b
eval (Not  b) = not (eval b)

