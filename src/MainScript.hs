module MainScript where

import           Prelude hiding (repeat)

import           HERMIT.API
import           HERMIT.API.Types

assumeRule :: String -> Shell ()
assumeRule ruleName = do
  eval $ "rule-to-lemma " ++ show ruleName
  shellEffect $ proveLemma (LemmaName ruleName)
  proofCmd assume

fullBetaReduce :: Rewrite LCore
fullBetaReduce = betaReduce >>> letSubst

-- Experimental de Bruijn transformation (WIP)
-- TODO: Find a better/more correct way to increment the de Bruijn indices.
deBruijn :: Rewrite LCore
deBruijn =
  serialise
    [ anyBU $ lemmaForward ">>=-subst" >>> try (anyBU $ lemmaForward "de-bruijn-succ")
    , anyBU $ fullBetaReduce
    ]

script :: Shell ()
script = do
  apply flattenModule
  mapM_ assumeRule
        [ "led-to-ledE"
        , "lower-button"
        , ">>=-assoc"
        , ">>=-left-id"
        , "commute-lit-not"
        , "lit-of-unLit"

        , "If-intro/>>case"
        , "If-intro/case>>"
        , "If-intro/return-case"
        , "If-intro/Action-case"
        , "If-intro/>>=case"
        , "If-intro/case>>="
        , "If-intro/Loop-case"
        , "If-intro/If-1"
        , "If-intro/If-2"
        , "If-intro/If-3"

        -- Lambdas
        , ">>=-subst"
        , "de-bruijn-succ"
        ]

  setPath $ rhsOf "main"

  -- *** Introduce ledE and buttonE ***
  apply . repeat $ foldr1 (>+>)
    [ anyBU $ lemmaForward "led-to-ledE"
    , serialise $ map anyBU
        [ lemmaForward "lower-button"
        , lemmaForward ">>=-assoc"
        ]
    , repeat . foldr1 (>+>) $ map anyBU
        [ lemmaForward "commute-lit-not"

        , fullBetaReduce
        , lemmaForward ">>=-left-id"

        , fullBetaReduce
        , lemmaForward "lit-of-unLit"
        ]
    ]

  -- *** Transform ifs ***
  apply . repeat $ foldr1 (>+>)
    [ anyBU inlineCaseAlternative -- Inline `wild`s
    , anyBU $ lemmaForward "If-intro/case>>"

    -- NOTE: Not tested:
    , anyBU $ lemmaForward "If-intro/>>case"
    , anyBU $ lemmaForward "If-intro/return-case"
    , anyBU $ lemmaForward "If-intro/Action-case"
    , anyBU $ lemmaForward "If-intro/>>=case"
    , anyBU $ lemmaForward "If-intro/case>>="
    , anyBU $ lemmaForward "If-intro/Loop-case"
    , anyBU $ lemmaForward "If-intro/If-1"
    , anyBU $ lemmaForward "If-intro/If-2"
    , anyBU $ lemmaForward "If-intro/If-3"
    ]

  apply . try $ anyBU etaReduce -- Take care of some lambdas

  -- apply deBruijn

