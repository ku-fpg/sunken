{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Types where

import           Control.Monad
import           Data.Proxy

-- | de Bruijn indices
type Name = Int

data LamTy

data E a where
  LitB   :: Bool                   -> E Bool
  LitI   :: Int                    -> E Int
  Not    :: E Bool                 -> E Bool
  Var    :: Name                   -> E a
  Lambda :: Name -> proxy t -> E r -> E LamTy -- XXX: Is this the type we want?
  R      :: R a                    -> E (R a)

getBinder :: E LamTy -> Name
getBinder (Lambda n _ _) = n

-- getBody :: E LamTy -> E r
-- getBody (Lambda _ _ b) = b

getArgProxy :: E LamTy -> Proxy t
getArgProxy _ = Proxy

subst :: E t -> E LamTy -> E r
subst arg (Lambda n _ body) = go body
  where
    go (Var n') = arg

-- TODO: Figure out capture avoidance.
abstract :: (E t -> E r) -> E LamTy
abstract f = Lambda 0 Proxy (f (Var 0))

data Action a where
  Button :: Int           -> Action (E Bool)
  Led    :: Int -> E Bool -> Action ()
  Wait   :: Int           -> Action ()
  LitA   :: E a           -> Action a

data R a where
  Action :: Action a             -> R a
  Bind   :: R a -> (E (a -> R a)) -> R b
  Return :: a                    -> R a
  Loop   :: R ()                 -> R ()
  If     :: E Bool -> R a -> R a -> R a

-- {-# NOINLINE Action #-}   -- XXX: How do we get rid of this warning?

-- instance Functor R where
--   fmap = liftM

-- instance Applicative R where
--   pure  = return
--   (<*>) = ap

-- instance Monad R where
--   return = Return
--   (>>=)  = Bind
--   a >> b = a >>= const b  -- XXX: Why is this needed to avoid a core lint warning?

