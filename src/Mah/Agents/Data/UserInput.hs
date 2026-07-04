module Mah.Agents.Data.UserInput where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.String (IsString(..))
import Mah.Agents.Data.Event (IsEvent(..))
import Mah.Agents.Pretty (Pretty(..))
import Witch (From(..), via)

newtype UserInput = UserInput Text
  deriving (Aeson.ToJSON, From Text, IsString, Show)

instance From String UserInput where
  from = via @Text

instance From UserInput Text

instance IsEvent UserInput where
  eventName _ = "UserInput"

instance Pretty UserInput where
  pretty (UserInput text) = "User input: " <> text
