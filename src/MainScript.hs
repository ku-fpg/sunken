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
    , "grab->id"
    , "commute-lit-grab"
    -- , "App-Lam-intro"   NOTE: (doesn't work right now. See hermit issue #169)
    , "add-to-addE"
    , "commute-lit-id"
    ]

  maxBV <- Local $ newIORef 0

  setPath $ rhsOf "main"

  -- Get rid of $s
  apply . repeat $ anyBU (fullBetaReduce <+ unfoldWith "$")

  -- ** Basic expression transformation **
  apply . anyBU $ lemmaForward "grab-intro/add"
  apply . try . repeat . anyBU $ lemmaForward "add-to-addE"
  apply . anyBU $ lemmaForward "commute-lit-grab"

  -- TODO: Eliminate the hardcoding like this
  apply . anyBU $ lemmaForward "commute-lit-id"
  apply . anyBU $ unfoldWith "id"

  test <- Local $ newIORef 0

  repeatUntilFail $ do
    apply . oneBU $ lemmaForward "grab->id"

    Local $ modifyIORef test succ

    testVal <- Local $ readIORef test
    Local $ putStr "--------- test = "
    Local $ print testVal

  return ()

