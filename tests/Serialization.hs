-- | ...
-- Serialization tests
-- ...

module Serialization 
  ( testCases
  ) where


import qualified Data.Yaml as YAML (decodeFileEither)
import Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Util (exampleProgramFilePath)


testsCases :: TestTree
testsCases = testGroup "Serialization" [ 
    yamlCanParseHelloWorld
  ]


yamlCanParseHelloWorld :: TestTree
yamlCanParseHelloWorld = 
  testCase "YAML > Deserialize > HelloWorld" $ do
    res <- YAML.decodeFileEither $ exampleProgramFilePath "hello-world"
    case res of
      Right program -> return ()
      Left  yamlErr -> assertFailure $ show yamlErr

