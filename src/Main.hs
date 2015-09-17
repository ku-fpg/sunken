module Main where
import           Shallow
import           Deep
import           Send
import           Types

import           GHC.Prim

import           Data.Proxy

import           Control.Monad.Trans
import           Control.Monad.State.Strict

constTrue :: Bool -> Bool
constTrue = const True

main :: IO ()
main = send $ do
  loop $ do
    b <- button 0
    led 0 b
    led 1 (not b)

    if b
      then button 1 >>= led 2
      else led 3 (not (not (not b)))  -- To make this more complex

    -- led 2 (constTrue b)

    wait 100

-- unLit :: E Bool -> Bool
unLit :: E a -> a
unLit = undefined
{-# NOINLINE unLit #-}
-- {-# WARNING unLit "*** unLit should *not* appear in final generated code! ***" #-}


  -- | Used for convenience so we don't have to
  --   spell out each case every time.
  --   Introduced at the beginning by the HERMIT
  --   shell script and then eliminated at the end.
  --   TODO: See if there's a better way.
grab :: a -> a
grab _ = error "No grabs should be in the generated code"
{-# NOINLINE grab #-}

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

{-# RULES "grab-elim" [~]
      forall x.
        grab x = x
  #-}

{-# RULES "grab-intro/>>" [~]
      forall (x :: R a) (y :: R b).
        x >> y
          =
        grab x >> grab y
  #-}

{-# RULES "grab-intro/>>=" [~]
      forall (m :: R a) (f :: a -> R b).
        m >>= f
          =
        grab m >>= grab f
  #-}

{-# RULES "grab-intro/return" [~]
      forall x.
        return x
          =
        return (grab x)
  #-}

{-# RULES "grab-intro/Action" [~]
      forall a.
        Action a
          =
        Action (grab a)
  #-}

{-# RULES "grab-intro/Loop" [~]
      forall x.
        Loop x
          =
        Loop (grab x)
  #-}

{-# RULES "grab-intro/If" [~]
      forall c t f.
        If c t f
          =
        If c (grab t) (grab f)
  #-}

  -- TODO: Make sure this definition makes sense:
doState :: Monad m => StateT s m a -> StateT s m (m a)
doState s = do
  a <- get
  lift . return $ evalStateT s a

{-# RULES "If-intro" [~]
      forall r (t :: R a) (f :: R a).
        grab (case unLit r of True -> t ; False -> f)
          =
        grab (do { t' <- doState t; f' <- doState f; lift (If r t' f') })
  #-}

--
-- Lambdas
--

{-# RULES "Lam-intro" [~]
      forall f.
        grab f
          =
        App (Lam Proxy UniquePlaceholder (f (Var UniquePlaceholder)))
  #-}

