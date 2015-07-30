{-# LANGUAGE GADTs #-}

module Types where

import           Control.Monad

data E a where
  LitB :: Bool -> E Bool
  LitI :: Int  -> E Int
  Not  :: E Bool -> E Bool

data Action a where
  Button :: Int -> Action (E Bool)
  Led    :: Int -> E Bool -> Action ()
  Wait   :: Int -> Action ()
  WaitE  :: E Int -> Action ()

data R a where
  Action :: Action a -> R a
  Bind   :: R a -> (a -> R b) -> R b
  Return :: a -> R a
  Loop   :: R () -> R ()
  IfE    :: E Bool -> R a -> R a -> R a

instance Functor R where
  fmap = liftM

instance Applicative R where
  pure  = return
  (<*>) = ap

instance Monad R where
  return = Return
  (>>=)  = Bind

