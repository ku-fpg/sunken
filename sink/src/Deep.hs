-- The deep API
module Deep
  where

import           Types

buttonE :: Int -> R (E Bool)
buttonE = Action . Button

ledE :: Int -> E Bool -> R ()
ledE ledNum = Action . Led ledNum

notE :: E Bool -> E Bool
notE = Not

loop :: R () -> R ()
loop = Loop

