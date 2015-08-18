module MainScript where

import           HERMIT.API
import           HERMIT.API.Types

assumeRule :: String -> Shell ()
assumeRule ruleName = do
  eval $ "rule-to-lemma " ++ show ruleName
  shellEffect $ proveLemma (LemmaName ruleName)
  proofCmd assume

fullBetaReduce :: Rewrite LCore
fullBetaReduce = betaReduce >>> letSubst

script :: Shell ()
script = do
  mapM_ assumeRule
        [ "led-to-ledE"
        , "lower-button"
        , ">>=-assoc"
        , ">>=-left-id"
        , "commute-lit-not"
        , "lit-of-unLit"
        ]

  setPath $ rhsOf "main"

  mapM_ (apply . anyBU . lemmaForward)
        [ "led-to-ledE"
        , "lower-button"
        , ">>=-assoc"
        , "commute-lit-not"
        ]

  apply . anyBU $ fullBetaReduce
  apply . anyBU $ lemmaForward ">>=-left-id"

  apply . anyBU $ fullBetaReduce
  apply . anyBU $ lemmaForward "lit-of-unLit"

