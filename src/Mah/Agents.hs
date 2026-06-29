{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Mah.Agents (main) where

import Control.Monad.Hefty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Text (Text)
import Prelude hiding (read)
import Witch (From(..), via)

-- * State

newtype AgentId = AgentId Int
  deriving (Eq, Ord)

newtype AgentThought = AgentThought Text
  deriving newtype (From Text)

instance IsString AgentThought where
  fromString = via @Text

newtype AgentResolution = AgentResolution Text
  deriving newtype (From Text)

instance IsString AgentResolution where
  fromString = via @Text

-- | Context available to an agent.
data AgentContext = AgentContext
  { events :: [Event]
  -- | Current tasks in progress.
  , subTasks :: Map AgentId Task
  -- | Current task in progress.
  , task :: Maybe Task
  }

emptyAgentContext :: AgentContext
emptyAgentContext = AgentContext
  { events = []
  , subTasks = Map.empty
  , task = Nothing
  }

-- | A side-effect the harness needs to perform.
data Command
  = Command_CallAgent AgentId
  | Command_RunTool AgentId ToolName ToolArgs
  | Command_WriteConsole Text

-- | Something that has happened.
data Event
  = Event_AgentInvocation
  | Event_AgentResolution AgentResolution
  | Event_AgentThought AgentThought
  | Event_AgentSpawned AgentId AgentId Task
  | Event_ToolCall ToolName ToolArgs
  | User_AgentCancel
  | User_Input Text

data HarnessState = HarnessState
  { events :: Map AgentId [Event]
  , tasks :: Map AgentId Task
  }

emptyHarnessState :: HarnessState
emptyHarnessState = HarnessState
  { events = Map.empty
  , tasks = Map.empty
  }

newtype Task = Task Text

instance From Task Text

newtype ToolArgs = ToolArgs Text

newtype ToolName = ToolName Text

-- * Effects

data Agent :: Effect where
  CallAgent :: AgentContext -> Agent f [Event]
makeEffectF ''Agent

runAgent :: Emb IO :> es => Eff (Agent : es) ~> Eff es
runAgent = interpret \case
  CallAgent agentContext -> pure
    [ Event_AgentThought "I am thinking it would be nice to be real."
    , Event_AgentResolution "I found the result :)"
    ]

data Console :: Effect where
  -- | Read any queued user input.
  ReadConsole :: Console f (Maybe Text)
  -- | Write some text to the console.
  WriteConsole :: Text -> Console f ()

makeEffectF '' Console

runConsole :: Emb IO :> es => Eff (Console : es) ~> Eff es
runConsole = interpret \case
  ReadConsole -> liftIO $ Just . from <$> getLine
  WriteConsole text -> liftIO $ putStrLn $ from text

-- * Logic

agentContext :: AgentId -> HarnessState -> AgentContext
agentContext agentId HarnessState{..} = AgentContext
  { events = agentEvents
  , subTasks = Map.fromList
      [ (subAgentId, activeTasks)
      -- For all sub-agents of the current agent.
      | Event_AgentSpawned agentId' subAgentId _ <- agentEvents
      -- Where the sub-agent is still active.
      , Just activeTasks <- [Map.lookup subAgentId tasks]
      ]
  , task = Map.lookup agentId tasks
  }
 where
  agentEvents = fromMaybe [] $ Map.lookup agentId events

applyEvent :: AgentId -> HarnessState -> Event -> HarnessState
applyEvent agentId state event =
  let state' = recordEvent agentId state event
  in case event of
    Event_AgentResolution _ ->
      -- TODO the LLM should control the task list in it's output.
      state' { tasks = Map.delete agentId state'.tasks }
    User_AgentCancel ->
      state' { tasks = Map.delete agentId state'.tasks }
    Event_AgentSpawned _parent subAgentId newTask ->
      state' { tasks = Map.insert subAgentId newTask state'.tasks }
    _ -> state'

-- | The pure brain of the harness.
handleEvent :: AgentId -> HarnessState -> Event -> (HarnessState, [Command])
handleEvent agentId state event =
  let state' = applyEvent agentId state event
      commands = case event of
        User_Input text ->
          [ Command_WriteConsole $ "User said: " <> text
          , Command_CallAgent agentId
          ]
        Event_ToolCall name args ->
          [ Command_RunTool agentId name args ]
        Event_AgentSpawned _ subId task ->
          [ Command_WriteConsole $ "Spawned sub-agent to " <> from task
          ]
        Event_AgentResolution (AgentResolution text) ->
          [ Command_WriteConsole $ "Agent says: " <> text ]
        _ -> []
  in (state', commands)

recordEvent :: AgentId -> HarnessState -> Event -> HarnessState
recordEvent agentId state event =
  let appendEvent Nothing = Just [event]
      appendEvent (Just events) = Just $ event : events
  in  state { events = Map.alter appendEvent agentId state.events }

step :: (Agent :> es, Console :> es) => AgentId -> HarnessState -> Eff es HarnessState
step agentId state = do
  readConsole >>= \case
    Nothing -> pure ()
    Just userInput -> do
      writeConsole $ "Read user input: " <> userInput
  pure state

main :: IO ()
main = runEff . runAgent . runConsole $ do
  writeConsole "Welcome. How may I help you?"
  _ <- step (AgentId 0) emptyHarnessState
  pure ()
