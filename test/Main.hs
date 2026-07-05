module Main (main) where

import Agent.Bob.Prelude
import Test.Config qualified
import Test.Event qualified
import Test.Hspec (hspec)
import Test.ProjectRoot qualified
import Test.Schema qualified

main :: IO ()
main = hspec $ do
  Test.Event.spec
  Test.Schema.spec
  Test.Config.spec
  Test.ProjectRoot.spec
