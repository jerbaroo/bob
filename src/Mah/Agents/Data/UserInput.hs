module Mah.Agents.Data.UserInput where

import Data.Text (Text)
import Data.String (IsString(..))
import Mah.Agents.Data.Event (IsEvent(..))
import Mah.Agents.Pretty (Pretty(..))
import Witch (From(..), via)

newtype UserInput = UserInput Text
  deriving (From Text, Show)

instance From String UserInput where
  from = via @Text

instance From UserInput Text

instance IsEvent UserInput

instance IsString UserInput where
  fromString = from

instance Pretty UserInput where
  pretty (UserInput text) = "User input: " <> text
