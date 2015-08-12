{-# LANGUAGE OverloadedStrings #-}
import           Graphics.Blank hiding (send, wait, eval)
import qualified Graphics.Blank as Blank

import           Web.KeyCode

import           Control.Monad.Reader
import           Control.Monad.State

import           Control.Concurrent

import           Control.Lens

import           Data.Maybe (fromMaybe)

import           Deep
import           Shallow
import           Server


main :: IO ()
main = do
  send $ do
    loop $ do
      b <- button 0
      led 0 b
      led 1 (not b)
      -- wait 100


-- Shallow->Deep transformation:

{-# RULES "ledE-intro" forall i b . led i b = ledE i (lit b) #-}

{-# RULES "lit-of-unLit" [~]
      forall x.
        lit (unLit x) = x
  #-}

{-# RULES "lower-button" [~]
      forall i f.
        button i >>= f
          =
        buttonE i >>= (\r -> f (unLit r))
  #-}


{-# RULES "eval-intro" [~]
      forall r.
        return (unLit r)
          =
        eval r
 #-}

{-# RULES "commute-not" [~]
      forall b.
        lit (not b)
          =
        notB (lit b)
  #-}

{-# RULES "lit-unLit" [~]
      forall b.
        lit (unLit b) = b
  #-}

