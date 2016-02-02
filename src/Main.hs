{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           Shallow
import           Deep
import           Types
import           Eval

import           Data.Proxy

import           GHC.Exts

import Data.Char

test :: Int -> Int
test x =
  case x of
    10 -> x * 2
    _  -> x + 1

main :: IO ()
main = print . eval $
  add 10 (test 3)

grab :: a -> a
grab = error "grab: No grabs should be in generated code"
{-# NOINLINE grab #-}

unLit :: E a -> a
unLit = error "unLit"
{-# NOINLINE unLit #-}

{-# RULES "intro-evalE" [~]
      forall x.
        eval x
          =
        evalE (lit x)
  #-}

{-# RULES "intro-liftFn" [~]
      forall rf f x.
        bind rf (lit (f x))
          =
        bind rf (App (Lam 0 (liftFn f (Var 0))) (lit x))
  #-}

{-# RULES "intro-add-bind" [~]
      forall a b.
        lit (add a b)
          =
        bind (\lb -> liftFn (`add` lb) (lit a)) (lit b)
  #-}

{-# RULES "addE-intro" [~]
      forall a b.
        bind (\lb -> liftFn (`add` lb) a) b
          =
        addE a b
  #-}

bind :: (Int -> E Int) -> (E Int -> E Int)
bind f = f . unLit

liftFn :: (Int -> Int) -> (E Int -> E Int)
liftFn f = Lit . f . unLit

