module Mah.Agents.Data.AgentOutput where

import Data.Aeson qualified as Aeson
import Mah.Agents.Data.AgentResolution (AgentResolution)
import Mah.Agents.Data.AgentTask (AgentTask)
import Mah.Agents.Data.AgentThought (AgentThought)
import Mah.Agents.Data.Event (IsEvent(..))
import Witch (from)

-- | The final output from the agent.
data AgentOutput = AgentOutput
  { resolution :: AgentResolution
  -- | Updated tasks for the current agent.
  , tasks :: [AgentTask]
  -- | Intermediary thoughts.
  , thoughts :: [AgentThought]
  } deriving Show

instance Aeson.FromJSON AgentOutput where
  parseJSON = Aeson.withObject "AgentOutput" $ \t ->
    pure $ AgentOutput (from $ show t) [] []

instance IsEvent AgentOutput where
  eventTasks AgentOutput{..} = Just tasks
