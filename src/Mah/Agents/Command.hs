module Mah.Agents.Command where

import Control.Monad (forM_)
import Control.Monad.Hefty (Eff, (:>))
import Data.Aeson qualified as Aeson
import Data.Effect.Reader (Ask, asks)
import Data.Effect.State (State, modify)
import Data.Map qualified as Map
import Prelude hiding (appendFile)
import Mah.Agents.Data.AgentId (AgentId)
import Mah.Agents.Data.Config (Config(..))
import Mah.Agents.Data.Event (Event, IsEvent(..))
import Mah.Agents.Data.HarnessState (HarnessState(..))
import Mah.Agents.Data.Level (Level(..))
import Mah.Agents.Data.Role (Role(..))
import Mah.Agents.Effect.Agent (Agent)
import Mah.Agents.Effect.Console (Console, writeSection)
import Mah.Agents.Effect.File (File, appendFile)
import Mah.Agents.Pretty (compact)
import Safe (lastMay)
import Witch (From(..))

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
        let f Nothing = Just newEvents
            f (Just events') = Just $ newEvents <> events'
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
