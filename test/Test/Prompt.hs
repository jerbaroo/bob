module Test.Prompt (spec) where

import Agent.Bob.Data.AgentInput (AgentInput(..))
import Agent.Bob.Data.UserInput (UserInput(..))
import Agent.Bob.Prelude
import Agent.Bob.Prompt.Build (contextBreakdown, promptSections, renderContextBreakdown)
import Data.Text qualified as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

-- | A minimal input: no events, no skills, no tasks.
baseInput :: AgentInput
baseInput = AgentInput
  { currentTime = UTCTime (fromGregorian 2026 7 7) 0
  , events = []
  , gitBranch = "main"
  , modelName = "TestModel"
  , modelSkills = []
  , modelSkillsActive = []
  , modelSoul = "Test soul."
  , modelTemperature = 0.5
  , modelTasks = []
  , operatingSystem = "linux"
  , projectRoot = "/tmp/project"
  }

spec :: Spec
spec = describe "context breakdown" $ do
  it "counts each non-empty prompt section by its exact length" $
    lookup "soul" (contextBreakdown baseInput)
      `shouldBe` Just (Text.length "Test soul.")

  it "omits empty sections" $ do
    -- No skills are activated, so the active_skills section is empty: it
    -- appears in 'promptSections' but not in the breakdown or the prompt.
    lookup "active_skills" (promptSections baseInput)
      `shouldBe` Just ""
    lookup "active_skills" (contextBreakdown baseInput)
      `shouldBe` Nothing

  it "counts zero session-history characters when there are no events" $
    lookup "session_history" (contextBreakdown baseInput) `shouldBe` Just 0

  it "counts the replayed message contents of events" $ do
    let input = baseInput{events = [from $ UserInput "hello", from $ UserInput "hi"]}
    lookup "session_history" (contextBreakdown input)
      `shouldBe` Just (Text.length "hello" + Text.length "hi")

  it "renders parts largest-first with percentages" $
    case Text.lines (renderContextBreakdown baseInput) of
      (header : biggest : _) -> do
        header `shouldSatisfy` Text.isPrefixOf "Context: "
        -- The response-format instructions dominate the minimal input.
        biggest `shouldSatisfy` Text.isInfixOf "response_format"
        biggest `shouldSatisfy` Text.isInfixOf "%)"
      _ -> expectationFailure "expected a header and at least one row"

  it "aligns the count and percentage columns" $ do
    let input = baseInput{events = [from $ UserInput "hello"]}
        rows = drop 1 $ Text.lines $ renderContextBreakdown input
        columnsOf marker = rows <&> Text.length . fst . Text.breakOn marker
        allEqual xs = length xs > 1 && all (== head xs) xs
    -- Every row's "(" (and hence its "%)") sits at the same column.
    columnsOf "(" `shouldSatisfy` allEqual
    columnsOf "%)" `shouldSatisfy` allEqual

  it "the environment section mentions model, branch, root and os" $ do
    let environment = fromMaybe "" $ lookup "environment" (promptSections baseInput)
    environment `shouldSatisfy` Text.isInfixOf "TestModel"
    environment `shouldSatisfy` Text.isInfixOf "main"
    environment `shouldSatisfy` Text.isInfixOf "/tmp/project"
    environment `shouldSatisfy` Text.isInfixOf "linux"
    environment `shouldSatisfy` Text.isInfixOf "2026-07-07"
