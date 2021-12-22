{-# LANGUAGE QuantifiedConstraints #-}
module Suite.Timeout (testTree) where

import Prelude
import Control.Monad (join)
import Data.Maybe (fromJust)
import Polysemy
import Polysemy.System.Timeout as T
import Control.Concurrent (threadDelay)
import Polysemy.Check
import Polysemy.Test
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((===))


testTree :: TestTree
testTree = testGroup "Polysemy.System.Timeout"
  [ unitTest "Timeout timed out" test_timeout_timeout
  , unitTest "Timeout passed" test_timeout_pass
  ]

test_timeout_pass :: UnitTest
test_timeout_pass = runTestAuto $ do
  let margin = 100
  base_time <- embed . generate $ chooseInteger (1000, 2000)
  intime <- embed . generate $ chooseInteger (0, base_time - margin)
  runTimeoutToFinalIO $ do
    let sleeper = do
          embed . threadDelay . fromInteger $ intime
          pure ()
    res <- timeout (fromInteger base_time) sleeper
    res === Just ()

test_timeout_timeout :: UnitTest
test_timeout_timeout = runTestAuto $ do
  let margin = 10
  base_time <- embed . generate $ chooseInteger (0, 1000)
  overtime <- embed . generate $ chooseInteger (base_time + margin, 2000)
  runTimeoutToFinalIO $ do
    let sleeper = do
          embed . threadDelay . fromInteger $ overtime
          pure ()
    res <- timeout (fromInteger base_time) sleeper
    res === Nothing

