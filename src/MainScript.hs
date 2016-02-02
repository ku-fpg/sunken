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

  setPath $ rhsOf "main"

  mapM_ assumeRule
    [ "intro-evalE"
    , "intro-liftFn"
    , "intro-add-bind"
    , "addE-intro"
    ]

  -- Get rid of $s
  apply . repeat $ anyBU (fullBetaReduce <+ unfoldWith "$")
  apply . oneTD $ unfoldWith "."

  apply . oneTD $ lemmaForward "intro-evalE"

  apply . anyBU $ lemmaForward "intro-add-bind"
  apply . anyBU $ lemmaForward "intro-liftFn"
  apply . anyBU $ lemmaForward "addE-intro"

  apply . oneBU $ unfoldWith "test"
  apply $ anyBU fullBetaReduce

  apply . anyBU $ unfoldWith "liftFn"
  apply . anyBU $ fullBetaReduce
  apply . anyBU $ unfoldWith "."
  apply . repeat . anyBU $ fullBetaReduce

