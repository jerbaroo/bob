module Test.Permission (spec) where

import Agent.Bob.Data.Permission (Decision(..), Permission(..), parseDecision, parsePermission)
import Agent.Bob.Prelude
import Data.Maybe (isNothing)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

spec :: Spec
spec = describe "permissions" $ do
  it "parses y/a as allow decisions, anything else denies" $ do
    parseDecision "y" `shouldBe` AllowOnce
    parseDecision " A " `shouldBe` AllowAlways
    parseDecision "n" `shouldBe` Deny
    parseDecision "yes please" `shouldBe` Deny
    parseDecision "" `shouldBe` Deny

  it "classifies permission lines as blank, grant or malformed" $ do
    isNothing (parsePermission "") `shouldBe` True
    isNothing (parsePermission "  # comment") `shouldBe` True
    case parsePermission "ReadFiles src/*" of
      Just (Right p) -> (into @Text p.toolName, p.param) `shouldBe` ("ReadFiles", "src/*")
      _ -> expectationFailure "expected a grant"
    case parsePermission "ReadFiles" of
      Just (Left raw) -> raw `shouldBe` "ReadFiles"
      _ -> expectationFailure "expected a malformed line"
