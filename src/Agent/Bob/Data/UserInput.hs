module Agent.Bob.Data.UserInput where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.String (IsString(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Message (Message(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Pretty (Pretty(..))
import Witch (From(..), via)

newtype UserInput = UserInput Text
  deriving (Aeson.FromJSON, Aeson.ToJSON, From Text, IsString, Show)

instance From String UserInput where
  from = via @Text

instance From UserInput Text

instance IsEvent UserInput where
  eventName _ = "UserInput"
  eventToMessage (UserInput text) =
    Just Message{content=text, role=User, thinking=Nothing}

instance Pretty UserInput where
  pretty (UserInput text) = "User input: " <> text
