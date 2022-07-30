-- | ...
-- Functions to support tests
-- ...

module Util 
  ( exampleFilePath
  ) where


exampleProgramFilePath :: Text -> FilePath
exampleProgramFilePath example = 
     "./examples/" 
  <> example 
  <> "program.yaml"
