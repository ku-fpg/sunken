module MainScript where

import           Prelude hiding (repeat)

import           HERMIT.API
import           HERMIT.API.Types

import           Data.IORef

import           Control.Monad (forever)

assumeRule :: String -> Shell ()
assumeRule ruleName = do
  eval $ "rule-to-lemma " ++ show ruleName
  shellEffect $ proveLemma (LemmaName ruleName)
  proofCmd assume

fullBetaReduce :: Rewrite LCore
fullBetaReduce = betaReduce >>> letSubst

repeatUntilFail :: Shell a -> Shell ()
repeatUntilFail (Fail _) = return ()
repeatUntilFail action   = r
  where
    r = action >> r

script :: Shell ()
script = do
  apply flattenModule

  mapM_ assumeRule
    [ "grab-intro/add"
    , "grab-intro/fn-call"
    , "grab->id"
    , "commute-lit-grab"
    -- , "grab-intro/evalE"
    , "App-Lam-intro"
    , "Var-succ"
    , "Lam-succ"
    , "Lam-intro"
    , "add-to-addE"
    , "join-grabs"
    , "commute-lit-id"
    , "release-grab"
    ]

  maxBV <- Local $ newIORef 0

  setPath $ rhsOf "main"

  -- Get rid of $s
  apply . repeat $ anyBU (fullBetaReduce <+ unfoldWith "$")
  apply . oneTD $ unfoldWith "."

  -- apply . oneBU $ lemmaForward "grab-intro/evalE"

  -- ** Basic expression transformation **
  apply . anyBU $ lemmaForward "grab-intro/add"
  apply . try . repeat . anyBU $ lemmaForward "add-to-addE"
  apply . anyBU $ lemmaForward "commute-lit-grab"

  -- ** Lambda transformation **
  apply . anyBU $ lemmaForward "commute-lit-id"
  apply . oneBU $ lemmaForward "grab-intro/fn-call"
  apply . oneBU $ lemmaForward "Lam-intro"

  -- -- ** Finish up **
  apply . anyBU $ lemmaForward "release-grab"

