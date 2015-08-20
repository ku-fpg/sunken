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

-- | Find and deepen an "if" (actually a case match on an unLit application)
deepenIf :: Rewrite LCore
deepenIf =
  focus (consider CaseOf)
        undefined

-- | lit (not b) ==> notE (lit b)
commuteLitNot :: Rewrite LCore
commuteLitNot = undefined

lemmaBU :: LemmaName -> Rewrite LCore
lemmaBU = anyBU . lemmaForward

-- lemmaBackBU :: LemmaName -> Rewrite LCore

-- | Apply rewrite to both case alts of a Core "if"
bothIfAlts :: Rewrite LCore -> Rewrite LCore
bothIfAlts r =
  serialise
    [ pathR [caseAlt 0] r
    , pathR [caseAlt 1] r
    ]

script :: Shell ()
script = do
  mapM_ assumeRule
        [ "led-to-ledE"
        , "lower-button"
        , ">>=-assoc"
        , ">>=-left-id"
        , "commute-lit-not"
        , "lit-of-unLit"
        , "If-intro/Action"
        ]

  setPath $ rhsOf "main"

  apply . serialise $ map anyTD
        [ lemmaForward "led-to-ledE"
        , lemmaForward "lower-button"
        , lemmaForward ">>=-assoc"
        ]

  apply . repeat . foldr1 (<+) $ map anyBU
       [ lemmaForward "commute-lit-not"

       , fullBetaReduce
       , lemmaForward ">>=-left-id"

       , fullBetaReduce
       , lemmaForward "lit-of-unLit"
       ]

  -- *** Handle `if`s ***
  apply . focus (consider CaseOf) . serialise $
    [ bothIfAlts (oneTD unfold)
    -- Float `case` inside of `Action` constructor application
    , anyBU caseFloatIn
    , smash -- Inline `wild`s
    , anyBU $ lemmaForward "If-intro/Action"
    ]

