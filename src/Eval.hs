{-# LANGUAGE GADTs #-}
module Eval
  (eval
  ,evalE
  )
  where

import           Types

eval :: a -> a
eval = id

evalE :: E a -> a
evalE (Add a b) = evalE a + evalE b
evalE (Lit a)   = a
-- evalE (App (Lam argName body) x) =
--   _ $ evalE (subst argName x body)
-- evalE (App f _) = -- TODO: Improve error message
--   error "Tried to apply a non-function"

-- evalE lam@(Lam argName body) =
--   error "Cannot eval a Lam"
--   -- evalE . subst argName body . _
-- evalE var@(Var _)   =
--   error "Cannot eval a Var"


-- -- This uses naive substitution that assumes
-- -- names are globally unique.
-- subst :: Int -> E a -> E a -> E a
-- subst argName x (Var v)
--   | v == argName = x
--   | otherwise    = Var v
-- subst argName x (Add a b) =
--   Add (subst argName x a)
--       (subst argName x b)

