-- The deep API
module Deep
  where

import           Types

import           Control.Monad.Trans
import           Control.Monad.State.Strict

buttonE :: Int -> R (E Bool)
buttonE = lift . Action . Button

ledE :: Int -> E Bool -> R ()
ledE ledNum = lift . Action . Led ledNum

notE :: E Bool -> E Bool
notE = Not

lit :: Bool -> E Bool
lit = LitB

loop :: R () -> R ()
loop r = do
  n <- get
  lift . Loop $ evalStateT r n

