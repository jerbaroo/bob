module Agent.Bob.Command where

import Agent.Bob.Data.AgentId (AgentId)
import Agent.Bob.Data.Config (Config(..))
import Agent.Bob.Data.Event (Event, IsEvent(..))
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Agent (Agent)
import Agent.Bob.Effect.Console (Console, writeSection)
import Agent.Bob.Effect.File (File, appendFile)
import Agent.Bob.Prelude hiding (appendFile)
import Agent.Bob.Pretty (compact)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map

-- | The effects every command is allowed to use.
type Harness es =
  ( Agent :> es
  , Ask Config :> es
  , Console :> es
  , File :> es
  , State HarnessState :> es
  )

-- | An existential wrapper over any concrete command.
data Command = forall c. (Aeson.ToJSON c, IsCommand c, IsEvent c) => Command c

instance (Aeson.ToJSON c, IsCommand c, IsEvent c) => From c Command where
  from = Command

instance Aeson.ToJSON Command where
  toJSON (Command c) = Aeson.toJSON c

instance IsEvent Command where
  eventName (Command c) = "Command:" <> eventName c
  eventTasks (Command c) = eventTasks c
  eventToMessage (Command c) = eventToMessage c

-- | Something the harness can do.
class IsCommand c where
  type Result c
  runCommand :: Harness es => AgentId -> c -> Eff es (Result c, [Event])

-- | Update the harness state with new events.
recordEvents
  :: (Ask Config :> es, Console :> es, File :> es, State HarnessState :> es)
  => AgentId -> [Event] -> Eff es ()
recordEvents agentId newEvents = do
  modify $ \HarnessState{..} -> HarnessState
    { events =
        -- 'newEvents' arrives in chronological order; the stored list is
        -- newest-first, so reverse the batch before prepending.
        let f Nothing = Just $ reverse newEvents
            f (Just events') = Just $ reverse newEvents <> events'
        in Map.alter f agentId events
    , tasks = maybe tasks (Map.singleton agentId) $
        eventTasks =<< lastMay newEvents
    , ..
    }
  memoryPath <- asks (.memoryPath)
  forM_ newEvents $ \event -> do
    writeSection System Meta $ "Recorded event: " <> eventName event
    appendFile memoryPath $ compact event <> "\n"

-- | Run a command, record the events, and return the command result.
step
  :: (Aeson.ToJSON c, Harness es, IsCommand c, IsEvent c)
  => AgentId -> c -> Eff es (Result c)
step agentId command = do
  (result, events) <- runCommand agentId command
  recordEvents agentId $ from (Command command) : events
  pure result
