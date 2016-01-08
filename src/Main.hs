module Main where
import           Shallow
import           Deep
import           Types
import           Eval

main :: IO ()
main = print . evalE $
  add 1 ((\x -> add x 10) 3)

