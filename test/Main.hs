import Prelude
import Test.Tasty
import qualified Suite.Timeout as ST (testTree)

main = defaultMain $
  testGroup "System effects"
  [ ST.testTree
  ]

