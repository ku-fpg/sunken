module Main where

import           Client
import           Server

main :: IO ()
main = runServer . send $
  loop $ do
    bE <- buttonE 0
    ledE 0 bE
    ledE 1 (notE bE)
    wait 100

