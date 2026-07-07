module Test.Session (spec) where

import Agent.Bob.Prelude
import Agent.Bob.Session (branchSessions, mkSessionName, )
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Test.Hspec (Spec, describe, it, shouldBe)

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 7 7) (12 * 3600 + 34 * 60 + 56)

spec :: Spec
spec = describe "sessions" $ do
  it "names sessions <root>_<branch>_<timestamp>_<uuid>" $
    mkSessionName "bob" "feat-x_y" epoch "uuid-1"
      `shouldBe` "bob_feat-x-y_20260707-123456_uuid-1"

  it "drops root and branch outside a git repository" $
    mkSessionName "bob" "default" epoch "uuid-1"
      `shouldBe` "20260707-123456_uuid-1"

  it "lists this branch's and custom-named sessions, newest first" $
    branchSessions "bob" "main"
      [ "bob_main_20260707-000001_a"
      , "bob_other_20260707-000003_b"
      , "bob_main_20260707-000002_c"
      , "my-experiment"
      ]
      `shouldBe`
      [ "my-experiment"
      , "bob_main_20260707-000002_c"
      , "bob_main_20260707-000001_a"
      ]
