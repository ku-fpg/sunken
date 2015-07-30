{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client where

import           Control.Monad

import           Server
import           Types

buttonE :: Int -> R (E Bool)
buttonE = Action . Button

ledE :: Int -> E Bool -> R ()
ledE i b = Action $ Led i b

loop :: R () -> R ()
loop = Loop

wait :: Int -> R ()
wait = Action . Wait

waitE :: E Int -> R ()
waitE = Action . WaitE

ifE :: E Bool -> R a -> R a -> R a
ifE = IfE

litB :: Bool -> E Bool
litB = LitB

litI :: Int -> E Int
litI = LitI

notE :: E Bool -> E Bool
notE = Not

send :: R a -> Remote a
send (Return x) = return x
send (Bind m f) = send m >>= send . f
send (Action a) = runCommand a
send (Loop m  ) = forever (send m)
send (IfE eb t f) = do
  b <- send (Return eb)
  if evalE b   -- XXX: Is this the right way to do it?
    then send t
    else send f

