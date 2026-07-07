module Agent.Bob.Command.CallAgent where

import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.AgentId (AgentId)
import Agent.Bob.Data.AgentInput (AgentInput(..))
import Agent.Bob.Data.AgentOutput (AgentMessageContent(..), AgentOutput(..), AgentOutputMeta(..))
import Agent.Bob.Data.FinishReason (FinishReason(..))
import Agent.Bob.Data.Config (Config, ConfigF(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Message (Message(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.Skill (Skill(..))
import Agent.Bob.Effect.Agent (callAgent)
import Agent.Bob.Effect.Clock (Clock, getTime)
import Agent.Bob.Effect.Console (Console, writeSection)
import Agent.Bob.Effect.File (File, readFileMaybe)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.Prompt.Build (buildPrompt, renderContextBreakdown)
import Agent.Bob.Skill (getSkills)
import Agent.Bob.Soul (getSoul)
import Data.Aeson qualified as Aeson
import Data.List (partition)
import Data.Map qualified as Map
import System.Info qualified

-- | Call the agent with a prompt built from the recorded events.
data CallAgent = CallAgent

instance Aeson.ToJSON CallAgent where
  toJSON CallAgent = "CallAgent"

instance IsCommand CallAgent where
  type Result CallAgent = AgentOutput
  runCommand agentId CallAgent = do
    agentInput <- mkAgentInput agentId
    writeSection System Meta $ renderContextBreakdown agentInput
    writeSection System Debug $ "Prompt sent to model:\n" <> pretty (buildPrompt agentInput)
    agentOutput@AgentOutput{..} <- callAgent agentInput
    writeSection Assistant Debug $ "Raw output from model:\n" <> pretty raw
    writeSection Assistant Meta $ pretty agentOutputMeta
    whenJust message.thinking $ writeSection Assistant Thinking . pretty
    writeSection Assistant Info $ from agentMessageContent.resolution <> case agentMessageContent.tasks of
      [] -> ""
      _  -> "\n\nRemaining tasks:\n" <> pretty agentMessageContent.tasks
    let AgentOutputMeta{..} = agentOutputMeta
        totalMs = round (timePrompt + timePredicted) :: Int
    writeSection Assistant Info $
         pretty totalMs <> "ms, "
      <> pretty tokensTotal <> " tokens ("
      <> pretty tokensCached <> " cached)"
    -- Stop and ToolCalls are normal; the rest signal a problem worth flagging.
    case finishReason of
      Length        -> writeSection Assistant Error "Output truncated (length limit reached)."
      ContentFilter -> writeSection Assistant Error "Output filtered by the engine; partial message kept."
      Other reason  -> writeSection Assistant Error $ "Unexpected finish reason: " <> reason
      _             -> pure ()
    pure (agentOutput, [from agentOutput])

instance IsEvent CallAgent where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

-- | Input to an agent derived from the harness state.
mkAgentInput
  :: ( Ask Config :> es, Clock :> es, Console :> es, File :> es
     , State HarnessState :> es )
  => AgentId -> Eff es AgentInput
mkAgentInput agentId = do
  Config{gitBranch, modelName, projectRoot} <- ask
  currentTime <- getTime
  let operatingSystem = from System.Info.os
  modelSoul <- getSoul
  allSkills <- getSkills
  activated <- gets (.userActivatedSkills)
  -- Activated skills are injected in full; the rest stay opt-in for the model.
  let (active, modelSkills) = partition ((`elem` activated) . (.name)) allSkills
  modelSkillsActive <- forM active \skill ->
    -- TODO avoid silent failure, print an error.
    (skill.name,) . fromMaybe "" <$> readFileMaybe skill.path
  HarnessState{..} <- get
  pure AgentInput
    { events = fromMaybe [] $ Map.lookup agentId events
    , modelTasks = fromMaybe [] $ Map.lookup agentId tasks
    , ..
    }
