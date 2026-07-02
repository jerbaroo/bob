module Mah.Agents.Data.UserInput where

import Data.Text (Text)
import Data.String (IsString(..))
import Mah.Agents.Data.Event (IsEvent(..))
import Witch (From(..), via)

newtype UserInput = UserInput Text
  deriving (From Text)

instance From String UserInput where
  from = via @Text

instance IsEvent UserInput

instance IsString UserInput where
  fromString = from
