{-# LANGUAGE GADTs #-}

module Types where

import           Control.Monad
import           Data.Proxy

data E a where
  -- Lit :: Int               -> E Int
  Lit :: a                 -> E a
  Add :: E Int -> E Int    -> E Int
  Lam :: Int -> E b        -> E (a -> b)
  App :: E (a -> b) -> E a -> E b
  Var :: Int               -> E a

