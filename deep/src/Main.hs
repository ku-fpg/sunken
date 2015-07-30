module Main where

import           Client
import           Server

main :: IO ()
main = runServer . send $
  loop $ do
    bE <- buttonE 0
    ledE 0 bE
    ledE 1 (notE bE)

    ifE bE
        (ledE 3 (litB True))
        (ledE 3 (litB False))

    wait 100

