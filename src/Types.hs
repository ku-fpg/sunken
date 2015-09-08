{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import           Control.Monad
import           Data.Proxy

-- Some lightweight singletons
data family Sing :: * -> *
data instance Sing Bool     = SBool
data instance Sing Int      = SInt
data instance Sing (a -> b) = Sing a :-> Sing b

-- Simple dependent sums (look into connection with natural transformations)
data tag ** f = forall a. !(tag a) :=> f a

data E a where
  LitB :: Bool              -> E Bool
  LitI :: Int               -> E Int
  Not  :: E Bool            -> E Bool
  Lam  :: Proxy a                  -- | Keep track of argument type
          -> [(String, Sing ** E)] -- | The variables being closed over by the
                                   --   lambda (XXX: Do we lose too much
                                   --   sharing if we also keep track of the
                                   --   expression in addition to the name?
                                   --   Maybe this should just be [String]?)
          -> String                -- | Argument name
          -> R b                   -- | Body of the lambda (NOTE: It is R ...)
          -> E (a -> b)
  Var  :: String            -> E a

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
  App    :: E (a -> b) -> E a -> R b

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

