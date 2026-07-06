module Test.File (spec) where

import Agent.Bob.Effect.File (appendFile, readFileMaybe, runFile, writeFile)
import Agent.Bob.Prelude hiding (appendFile, writeFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Effect.File" $
  it "reads then appends the same file without locking (regression test)" $
    -- Under lazy 'System.IO.readFile' the read handle stayed open until the
    -- returned contents were forced. When nothing forced them (e.g. a
    -- log message filtered out by log level) the following 'appendFile' to the
    -- same path crashed with "resource busy (file is locked)". Strict reads in
    -- 'runFile' close the handle before returning, so this sequence is safe.
    withSystemTempDirectory "agent-bob-file" $ \dir -> do
      let name = "log"
      runEff . runFile dir $ writeFile name "line1\n"
      result <- runEff . runFile dir $ do
        _unforced <- readFileMaybe name          -- deliberately not forced
        appendFile name "line2\n"
        readFileMaybe name
      result `shouldBe` Just "line1\nline2\n"
