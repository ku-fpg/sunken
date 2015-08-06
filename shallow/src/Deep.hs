{-# LANGUAGE GADTs #-}
module Deep
  (R
  ,R'
  ,E
  ,eval
  ,buttonE
  ,ledE
  ,lit
  ,unLit
  ,liftR
  ,notB
  )
  where

import           Control.Monad.Reader
import           Control.Monad.State
import           Graphics.Blank (DeviceContext)

type R = StateT ([Bool], [Bool]) (ReaderT DeviceContext IO)

data R' a
instance Functor R'
instance Applicative R'
instance Monad R'

data E a where
  LitB :: Bool -> E Bool
  Not  :: E Bool -> E Bool

eval :: E Bool -> R' Bool
eval = undefined

buttonE :: Int -> R' (E Bool)
buttonE = undefined

ledE :: Int -> E Bool -> R' ()
ledE = undefined

notB :: E Bool -> E Bool
notB = undefined

{-# NOINLINE lit #-}
lit :: Bool -> E Bool
lit = LitB

-- TODO: Use nullary type class trick to verify that this isn't in the final
--       generated code.
{-# NOINLINE unLit #-}
unLit :: E Bool -> Bool
unLit = undefined

liftR :: R' a -> R a
liftR = undefined

sendR :: R' a -> IO a
sendR = undefined

