-- The deep API
module Deep
  where

import           Types

addE :: E Int -> E Int -> E Int
addE = Add

lit :: Int -> E Int
lit = Lit

