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

    if b
      then led 3 True
      else led 3 (not (not (not (not b))))  -- To make this more complex

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
      forall (m :: R a) (f :: a -> R b) (g :: b -> R c).
        (m >>= f) >>= g
          =
        m >>= (\r -> f r >>= g)
  #-}

{-# RULES ">>=-left-id" [~]
      forall (x :: a) (f :: a -> R b).
        return x >>= f
          =
        f x
  #-}

{-# RULES "commute-lit-not" [~]
      forall b.
        lit (not b)
          =
        notE (lit b)
  #-}

{-# RULES "If-intro/Action" [~]
      forall r f t.
        Action (case unLit r of False -> f ; True -> t)
          =
        If r (Action f) (Action t)
  #-}

