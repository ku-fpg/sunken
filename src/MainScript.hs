module MainScript where

import           HERMIT.API

script :: Shell ()
script = do
  eval "rule-to-lemma led-to-ledE"
  shellEffect $ proveLemma "led-to-ledE"
  proofCmd assume

  setPath $ rhsOf "main"

  apply . anyBU $ lemmaForward "led-to-ledE"

