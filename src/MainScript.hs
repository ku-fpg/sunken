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
    [ 
    ]

grabSomething :: Rewrite LCore
grabSomething =
  foldr1 (<+)
    [ lemmaForward "grab-intro/>>"
    , lemmaForward "grab-intro/>>="
    , lemmaForward "grab-intro/return"
    , lemmaForward "grab-intro/Action"
    , lemmaForward "grab-intro/Loop"
    , lemmaForward "grab-intro/If"
    ]

elimGrabs :: Rewrite LCore
elimGrabs =
  anyBU $ lemmaForward "grab-elim"

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

        , "grab-elim"
        , "grab-intro/>>"
        , "grab-intro/>>="
        , "grab-intro/return"
        , "grab-intro/Action"
        , "grab-intro/Loop"
        , "grab-intro/If"

        , "If-intro"

        -- Lambdas
        , "MaxBV-intro"
        , "succ-MaxBV"
        , "Lam-intro"
        , "succ-Lam-BV"
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
  apply . anyBU $ grabSomething

  apply . repeat $ foldr1 (>+>)
    [ anyBU inlineCaseAlternative -- Inline `wild`s
    , anyBU $ lemmaForward "If-intro"
    ]

  apply . try $ anyBU etaReduce -- Take care of some lambdas

  -- TODO: Test this out
    -- *** de Bruijn transformation ***
  -- apply . try $ unfoldWith "$"   -- It's common to use "send $ ..." so we eliminate the $
  -- apply $ lemmaForward "MaxBV-intro"

  -- apply . oneTD $ lemmaForward "Lam-intro"

    -- TODO: See if this correctly increments the value in MaxBV
  -- kernelEffect top
  -- setPath $ rhsOf "main"
  -- apply . repeat $ lemmaForward "succ-MaxBV" >>> anyTD (lemmaForward "succ-Lam-BV") >>> anyTD (lemmaForward "Lam-intro")

  -- apply $ anyBU fullBetaReduce

    -- *** Clean up ***
  apply elimGrabs

