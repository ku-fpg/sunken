import           Shallow
import           Send

main :: IO ()
main = send $ do
  loop $ do
     b <- button 0
     led 0 b
     led 1 (not b)
     wait 100

