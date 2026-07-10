module Agent.Bob.Data.UserInputParsed where

import Data.Aeson ((.=))
import Agent.Bob.Data.Event (IsEvent(..), KnownEvent)
import Agent.Bob.Data.Message (Message(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.UserInput (UserInput(..))
import Agent.Bob.Prelude
import Agent.Bob.UserCommand (UserCommand(..))
import Data.Aeson qualified as Aeson
import Test.QuickCheck.Instances ()

data UserInputParsed
  = UserInputCommand UserCommand
  | UserInputForModel UserInput

instance Aeson.ToJSON UserInputParsed where
  toJSON u =
    let (tag :: Text, value) = case u of
          UserInputCommand userCommand -> ("command", Aeson.toJSON userCommand)
          UserInputForModel userInput -> ("input", Aeson.toJSON userInput)
    in  Aeson.object [ "tag" .= tag, "value" .= value ]

instance KnownEvent UserInputParsed

instance IsEvent UserInputParsed where
  eventTasks _ = Nothing
  eventToMessage UserInputCommand{} = Nothing
  eventToMessage (UserInputForModel userInput) =
    Just Message{content=from userInput, role=User, thinking=Nothing}
