module Main (main) where

import Agent.Bob.Prelude
import Test.Config qualified
import Test.Event qualified
import Test.File qualified
import Test.Hspec (hspec)
import Test.Permission qualified
import Test.ProjectRoot qualified
import Test.Prompt qualified
import Test.Schema qualified
import Test.Session qualified

main :: IO ()
main = hspec $ do
  Test.Event.spec
  Test.Schema.spec
  Test.Config.spec
  Test.ProjectRoot.spec
  Test.File.spec
  Test.Permission.spec
  Test.Prompt.spec
  Test.Session.spec
