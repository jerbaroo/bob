module Agent.Bob.Command.ReadConsole where

import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Effect.Console (readUserInput)
import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson

data ReadConsole = ReadConsole

instance IsCommand ReadConsole where
  type Result ReadConsole = UserInput
  runCommand _agentId ReadConsole = (id &&& (:[]) . from) <$> readUserInput

instance Aeson.ToJSON ReadConsole where
  toJSON ReadConsole = Aeson.String "ReadConsole"

instance IsEvent ReadConsole where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing
