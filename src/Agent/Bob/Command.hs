module Agent.Bob.Command where

import Agent.Bob.Data.AgentId (AgentId)
import Agent.Bob.Data.Config (Config)
import Agent.Bob.Data.Event (Event(..), IsEvent(..), KnownEvent(..), envelope)
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Clock (Clock, getTime)
import Agent.Bob.Effect.Console (Console, writeSection)
import Agent.Bob.Effect.File (File, appendFile)
import Agent.Bob.Effect.Harness (Harness)
import Agent.Bob.Prelude hiding (appendFile)
import Agent.Bob.Util.Aeson (textViaJSON)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map

-- | Something the harness can do.
class IsCommand c where
  type Result c
  runCommand :: Harness es => AgentId -> c -> Eff es (Result c, [Event])

-- | Update the harness state with new events.
recordEvents
  :: ( Ask Config :> es, Clock :> es, Console :> es, File :> es
     , State HarnessState :> es )
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
  sessionPath <- gets (.sessionPath)
  time <- getTime
  forM_ newEvents $ \event@(Event (_ :: e)) -> do
    writeSection System Meta $ "Recorded event: " <> eventName @e
    appendFile sessionPath $ textViaJSON (envelope time event) <> "\n"

-- | Run a command, record the events, and return the command result.
step
  :: (Aeson.ToJSON c, Harness es, IsCommand c, IsEvent c, KnownEvent c)
  => AgentId -> c -> Eff es (Result c)
step agentId command = do
  recordEvents agentId [Event command]
  (result, events) <- runCommand agentId command
  recordEvents agentId events
  pure result
