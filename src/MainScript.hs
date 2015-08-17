module MainScript where

import           HERMIT.API
import           HERMIT.API.Types

assumeRule :: String -> Shell ()
assumeRule ruleName = do
  eval $ "rule-to-lemma " ++ show ruleName
  shellEffect $ proveLemma (LemmaName ruleName)
  proofCmd assume

script :: Shell ()
script = do
  eval "set-pp-type Detailed"

  mapM_ assumeRule
        [ "led-to-ledE"
        , "lower-button"
        , ">>=-assoc"
        , "commute-lit-not"
        ]

  setPath $ rhsOf "main"

  mapM_ (apply . anyBU . lemmaForward)
        [ "led-to-ledE"
        , "lower-button"
        -- XXX: Not working:
        -- , ">>=-assoc"
        , "commute-lit-not"
        ]

