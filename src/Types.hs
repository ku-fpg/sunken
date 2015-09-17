{-# LANGUAGE GADTs #-}

module Types where

import           Control.Monad
import           Data.Proxy

import           Control.Monad.State.Strict

data Unique
  = Unique !Int
  | UniquePlaceholder -- | This is to be filled in with a 'Unique' later.

data E a where
  LitB :: Bool   -> E Bool
  LitI :: Int    -> E Int
  Not  :: E Bool -> E Bool
  Var  :: Unique -> E a

data Action a where
  Button :: Int           -> Action (E Bool)
  Led    :: Int -> E Bool -> Action ()
  Wait   :: Int           -> Action ()
  LitA   :: E a           -> Action a

data R' a where
  Action :: Action a                  -> R' a
  Bind   :: R' a -> (a -> R' b)       -> R' b
  Return :: a                         -> R' a
  Loop   :: R' ()                     -> R' ()
  If     :: E Bool -> R' a -> R' a    -> R' a
  App    :: R' (a -> b) -> E a        -> R' b
  Lam    :: Proxy a -> Unique -> R b  -> R' b -- XXX: Should this be R' (a -> b)?

type R = StateT UniqueSupply R'

type UniqueSupply = Int

newUnique :: R Unique
newUnique = do
  u <- Unique <$> get
  modify (+1)
  return u

-- {-# NOINLINE Action #-}   -- XXX: How do we get rid of this warning?

instance Functor R' where
  fmap = liftM

instance Applicative R' where
  pure  = return
  (<*>) = ap

instance Monad R' where
  return = Return
  (>>=)  = Bind
  a >> b = a >>= const b  -- XXX: Why is this needed to avoid a core lint warning?

