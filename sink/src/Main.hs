import           Shallow
import           Deep
import           Send
import           Types

main :: IO ()
main = send $ do
  loop $ do
     b <- button 0
     led 0 b
     led 1 (not b)
     wait 100

unLit :: E Bool -> Bool
unLit = undefined

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
        buttonE i >>= (\r -> return (unLit r))
  #-}

{-# RULES "evalRemote-intro" [~]
      forall r.
        return (unLit r)
          =
        evalRemote r
  #-}

{-# RULES "commute-lit-not" [~]
      forall b.
        lit (not b)
          =
        notE (lit b)
  #-}

