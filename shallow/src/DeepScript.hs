module DeepScript where

import           HERMIT.API
import           HERMIT.API.Types (LemmaName (..))

assumeRule :: String -> Shell ()
assumeRule ruleName = do
  eval $ "rule-to-lemma " ++ ruleName
  shellEffect $ proveLemma (LemmaName ruleName)
  apply $ try instDictionaries
  proofCmd assume

-- Manually fire a rule in the given function (GHC should probably do this
-- automatically most of the time, but I can't get that working at the moment).
fireRule :: Name -> LemmaName -> Shell ()
fireRule fnName lemmaName@(LemmaName ruleName) = do
  assumeRule ruleName

  scope $ do
    setPath $ rhsOf fnName
    apply . anyBU $ lemmaForward lemmaName

introCompose :: Rewrite LCore
introCompose = serialise
  [pathRs [lamBody] $
    [pathR [appArg] $
      abstract "r"  -- TODO: Generalize this to work for any variable name
    ,fold "."
    ]
  ,etaReduce
  ]

script :: Shell ()
script = do
  -- *** Step 1: lower-button
  fireRule "main" "lower-button"
    -- Put unLit in the right place
  scope $ do
    setPath . parentOfCoreTC $ applicationOf "unLit"
    apply betaReduce
    apply letSubst

  -- *** Step 2: lower led
    -- Introduce ledE
  fireRule "main" "ledE-intro"
  fireRule "main" "commute-not"  -- lit (not b) ==> notB (lit b)
  fireRule "main" "lit-unLit"

  fireRule "main" "liftR-hom_"

  assumeRule "liftR-hom"

  scope $ do
    setPath $ rhsOf "main"
    apply $ oneBU introCompose

    apply . oneBU $ lemmaForward "liftR-hom"

