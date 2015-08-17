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
{-# NOINLINE unLit #-}

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
      forall m f g.
        (m >>= f) >>= g
          =
        m >>= (\e -> f e >>= g)
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
