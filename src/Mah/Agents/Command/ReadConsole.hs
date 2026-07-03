module Mah.Agents.Command.ReadConsole where

import Control.Arrow ((&&&))
import Mah.Agents.Command (IsCommand(..))
import Mah.Agents.Data.Event (IsEvent)
import Mah.Agents.Data.UserInput (UserInput)
import Mah.Agents.Effect.Console (readUserInput)
import Mah.Agents.Pretty (Pretty(..))
import Witch (from)

data ReadConsole = ReadConsole

instance IsCommand ReadConsole where
  type Result ReadConsole = UserInput
  runCommand _agentId ReadConsole = (id &&& (:[]) . from) <$> readUserInput

instance IsEvent ReadConsole

instance Pretty ReadConsole where
  pretty ReadConsole = "Read user input"
