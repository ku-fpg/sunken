{-# LANGUAGE GADTs #-}
<<<<<<< Updated upstream
{-# LANGUAGE DataKinds #-}
=======
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
>>>>>>> Stashed changes

module Types where

import           Control.Monad
import           Data.Proxy

<<<<<<< Updated upstream
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
=======
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
>>>>>>> Stashed changes

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
<<<<<<< Updated upstream
=======
  App    :: E (a -> b) -> E a -> R b
>>>>>>> Stashed changes

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

