{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import           Shallow
import           Deep
import           Send
import           Types

import           GHC.Prim
import           Data.Proxy

main :: IO ()
main = send $ do
  loop $ do
    b <- button 0
    led 0 b
    led 1 (not b)

    if b
      then button 1 >>= led 2
      else led 3 (not (not (not (not b))))  -- To make this more complex

    wait 100

unLit :: E Bool -> Bool
unLit = undefined
{-# NOINLINE unLit #-}
-- {-# WARNING unLit "*** unLit should *not* appear in final generated code! ***" #-}

{-# RULES "led-to-ledE" [~]
      forall n i.
        led n i
          =
        ledE n (lit i)
  #-}

{-# RULES "lit-of-unLit" [~]
    forall x.
      lit (unLit x) = x
  #-}

{-# RULES "lower-button" [~]
      forall i.
        button i
          =
        do { x <- buttonE i; return (unLit x) }
  #-}

{-# RULES ">>=-assoc" [~]
      forall (m :: R a) (f :: a -> R b) (g :: b -> R c).
        (m >>= f) >>= g
          =
        m >>= (\r -> f r >>= g)
  #-}

{-# RULES ">>=-left-id" [~]
      forall (x :: a) (f :: a -> R b).
        return x >>= f
          =
        f x
  #-}

{-# RULES "commute-lit-not" [~]
      forall b.
        lit (not b)
          =
        notE (lit b)
  #-}

{-# RULES "If-intro/>>case" [~]
      forall r f t (m :: R a).
        m >> case unLit r of False -> f ; True -> t
          =
        m >> If r t f
  #-}


{-# RULES "If-intro/case>>" [~]
      forall r f t (m :: R a).
        (case unLit r of False -> f ; True -> t) >> m
          =
        If r t f >> m
  #-}

-- NOTE: These are untested:
{-# RULES "If-intro/return-case" [~]
      forall r f t.
        return (case unLit r of False -> f ; True -> t)
          =
        return (If r t f)
  #-}

{-# RULES "If-intro/Action-case" [~]
      forall r t f.
        Action (case unLit r of False -> f ; True -> t)
          =
        If r (Action t) (Action f)
  #-}

  -- XXX: Is it ok to introduce a lambda here?
{-# RULES "If-intro/>>=case" [~]
      forall (m :: R a) r (t :: a -> R b) (f :: a -> R b).
        m >>= case unLit r of False -> f ; True -> t
          =
        m >>= (\x -> If r (t x) (t x))
  #-}

{-# RULES "If-intro/case>>=" [~]
      forall r t f mf.
        (case unLit r of False -> f ; True -> t) >>= mf
          =
        (If r t f) >>= mf
  #-}

{-# RULES "If-intro/Loop-case" [~]
      forall r t f.
        Loop (case unLit r of False -> f ; True -> t)
          =
        Loop (If r t f)
  #-}

{-# RULES "If-intro/If-2" [~]
      forall r1 r2 t f1 f2.
        If r1 (case unLit r2 of False -> f1 ; True -> t) f2
          =
        If r1 (If r2 t f1) f2
  #-}

<<<<<<< Updated upstream
{-# RULES "If-intro/If-3" [~]
      forall r1 r2 t1 f t2.
        If r1 t1 (case unLit r2 of False -> f ; True -> t2)
          =
        If r1 t1 (If r2 f t2)
=======
{-# RULES "Lam-intro" [~]
      forall (f :: E a -> R b).
        grab f
          =
        App (Lam Proxy 0 (f (Var 0)))
  #-}

{-# RULES "succ-Lam-BV" [~]
      forall i (f :: E a -> R b).
        App (Lam Proxy i (f (Var i)))
          =
        App (Lam Proxy (succ i) (f (Var (succ i))))
>>>>>>> Stashed changes
  #-}

