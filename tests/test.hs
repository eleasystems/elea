-- | ...
-- Elea tests entrypoint
-- ...


import Test.Tasty
import Test.Tasty.HUnit

import qualified Serialization


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests" [   
    Serialization.testCases
  ]
