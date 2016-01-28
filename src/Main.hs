module Main where

import           Shallow
import           Deep
import           Types
import           Eval

import           Data.Proxy

-- add10 :: Int -> E Int
-- add10 = \x -> add x 10

main :: IO ()
main = print . evalE $
  add 10 (id 3)

grab :: a -> a
grab = error "grab: No grabs should be in generated code"
{-# NOINLINE grab #-}

-- This is a test RULE that can be removed:
{-# RULES "grab->id" [~]
      grab = id
  #-}

{-# RULES "grab-intro/add" [~]
      forall a b.
        add a b
          =
        add (grab a) (grab b)
  #-}

{-# RULES "commute-lit-grab" [~]
      forall x.
        lit (grab x)
          =
        grab (lit x)
  #-}

{-# RULES "App-Lam-intro" [~]
      forall f (x :: E a).
        grab f (grab x)
          =
        App (Lam 0 (f (Var 0))) x
  #-}

{-# RULES "Var-succ" [~]
      forall n.
        Var n
          =
        Var (succ n)
  #-}

{-# RULES "Lam-succ" [~]
      forall n x.
        Lam n x
          =
        Lam (succ n) x
  #-}

-- {-# RULES "grab-intro/evalE"
--       forall x.
--         evalE x
--           =
--         evalE (grab x)
--   #-}

-- TODO: See if it is possible to generalize this so that f :: a -> b
{-# RULES "grab-intro/fn-call" [~]
      forall (f :: a -> a) (x :: a).
        grab (f x)
          =
        (grab f) (grab x)
  #-}

{-# RULES "Lam-intro" [~]
      forall (f :: E a -> E b).
        grab f
          =
        App (Lam 0 (f (Var 0)))
  #-}

{-# RULES "add-to-addE" [~]
     forall a b.
       add a b
        =
       addE (lit a) (lit b)
  #-}

{-# RULES "join-grabs" [~]
      forall x.
        grab (grab x)
          =
        grab x
  #-}

{-# RULES "commute-lit-id" [~]
      forall (f :: forall a. a -> a) x.
        lit (f x)
          =
        f (lit x)
  #-}

{-# RULES "release-grab" [~]
      forall x.
        grab x = x
  #-}

