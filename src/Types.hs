{-# LANGUAGE GADTs #-}

module Types where

import           Control.Monad

data E a where
  LitB :: Bool -> E Bool
  LitI :: Int  -> E Int
  Not  :: E Bool -> E Bool
  App  :: (a -> b) -> a -> E b  -- | Used to recognize the lit (f (unLit r)) pattern
  -- Lam  :: E b -> E (a -> b)
  -- Var  :: Int -> E a        -- | de Bruijn indices
  -- Apply  :: E (a -> b) -> E a -> E b

data Action a where
  Button :: Int -> Action (E Bool)
  Led    :: Int -> E Bool -> Action ()
  Wait   :: Int -> Action ()
  LitA   :: E a -> Action a

data R a where
  Action :: Action a -> R a
  Bind   :: R a -> (a -> R b) -> R b
  Return :: a -> R a
  Loop   :: R () -> R ()
  If     :: E Bool -> R a -> R a -> R a

-- {-# NOINLINE Action #-}   -- XXX: How do we do this?

instance Functor R where
  fmap = liftM

instance Applicative R where
  pure  = return
  (<*>) = ap

instance Monad R where
  return = Return
  (>>=)  = Bind
  a >> b = a >>= const b  -- XXX: Why is this needed to avoid a core lint warning?

