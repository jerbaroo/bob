module Mah.Agents.UserCommand.Registry where

import Data.Aeson qualified as Aeson
import Data.List.Extra (firstJust)
import Data.Proxy (Proxy(..))
import Mah.Agents.Command (Command(..), IsCommand)
import Mah.Agents.Data.Event (IsEvent)
import Mah.Agents.Data.UserInput (UserInput)
import Mah.Agents.UserCommand (IsUserCommand(..))
import Mah.Agents.UserCommand.AgentCall (AgentCall)
import Mah.Agents.UserCommand.ConfigShow (ConfigShow)
import Mah.Agents.UserCommand.PermissionsShow (PermissionsShow)

-- | An existential over a user command type (not a value).
data SomeUserCommand
  = forall c. (Aeson.ToJSON c, IsCommand c, IsEvent c, IsUserCommand c)
  => SomeUserCommand (Proxy c)

-- | Every user command the harness knows how to run, tried in order:
-- 'AgentCall' parses any input, so it must stay last.
knownUserCommands :: [SomeUserCommand]
knownUserCommands =
  [ SomeUserCommand $ Proxy @ConfigShow
  , SomeUserCommand $ Proxy @PermissionsShow
  , SomeUserCommand $ Proxy @AgentCall
  ]

-- | The first of 'knownUserCommands' to parse the input, wrapped as a harness
-- 'Command' ready to execute. Never 'Nothing' in practice: 'AgentCall' parses
-- any input.
parseCommand :: UserInput -> Maybe Command
parseCommand input = firstJust parse knownUserCommands
 where
  parse (SomeUserCommand (_ :: Proxy c)) = Command <$> parseUserCommand @c input
