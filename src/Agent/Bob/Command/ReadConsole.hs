module Agent.Bob.Command.ReadConsole where

import Control.Arrow ((&&&))
import Data.Aeson qualified as Aeson
import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Effect.Console (readUserInput)
import Witch (from)

data ReadConsole = ReadConsole

instance IsCommand ReadConsole where
  type Result ReadConsole = UserInput
  runCommand _agentId ReadConsole = (id &&& (:[]) . from) <$> readUserInput

instance Aeson.ToJSON ReadConsole where
  toJSON ReadConsole = Aeson.String "ReadConsole"

instance IsEvent ReadConsole where
  eventName _ = "ReadConsole"
