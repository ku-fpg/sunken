module MainScript where

import           HERMIT.API
import           HERMIT.API.Types

assumeRule :: String -> Shell ()
assumeRule ruleName = do
  eval $ "rule-to-lemma \"" ++ ruleName ++ "\""
  shellEffect $ proveLemma (LemmaName ruleName)
  proofCmd assume

script :: Shell ()
script = do
  eval "set-pp-type Detailed"

  mapM_ assumeRule
        [ "led-to-ledE"
        , "lower-button"
        , ">>=-assoc"
        ]

  setPath $ rhsOf "main"

  apply . anyBU $ lemmaForward "led-to-ledE"
  apply . anyBU $ lemmaForward "lower-button"
  -- XXX: Not working:
  -- apply . anyBU $ lemmaForward ">>=-assoc"

