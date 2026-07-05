module Agent.Bob.Data.UserInput where

import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Test.QuickCheck.Instances ()
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Message (Message(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Pretty (Pretty(..))

newtype UserInput = UserInput Text
  deriving (Aeson.FromJSON, Aeson.ToJSON, Eq, From Text, IsString, Show)

instance Arbitrary UserInput where
  arbitrary = UserInput <$> arbitrary

instance From String UserInput where
  from = via @Text

instance From UserInput String where
  from = via @Text

instance From UserInput Text

instance IsEvent UserInput where
  eventTasks _ = Nothing
  eventToMessage (UserInput text) =
    Just Message{content=text, role=User, thinking=Nothing}

instance Pretty UserInput where
  pretty (UserInput text) = "User input: " <> text
