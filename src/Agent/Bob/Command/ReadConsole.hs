module Agent.Bob.Command.ReadConsole where

import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.UserInputParsed (UserInputParsed(..))
import Agent.Bob.Effect.Console (readUserInput)
import Agent.Bob.Prelude
import Agent.Bob.UserCommand.Registry (parseUserCommand')
import Data.Aeson qualified as Aeson

data ReadConsole = ReadConsole

instance IsCommand ReadConsole where
  type Result ReadConsole = UserInputParsed
  runCommand _agentId ReadConsole = do
    userInput <- readUserInput
    let userInputParsed = case parseUserCommand' userInput of
          Just userCommand -> UserInputCommand userCommand
          Nothing -> UserInputForModel userInput
    pure (userInputParsed, [from userInputParsed])

instance Aeson.ToJSON ReadConsole where
  toJSON ReadConsole = Aeson.String "ReadConsole"

instance IsEvent ReadConsole where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing
