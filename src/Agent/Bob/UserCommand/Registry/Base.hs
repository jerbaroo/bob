module Agent.Bob.UserCommand.Registry.Base where

import Data.Aeson qualified as Aeson
import Agent.Bob.Command (IsCommand)
import Agent.Bob.Data.Event (IsEvent)
import Agent.Bob.Prelude
import Agent.Bob.UserCommand (IsUserCommand)
import Agent.Bob.UserCommand.ActivateSkill (ActivateSkill)
import Agent.Bob.UserCommand.AgentCall (AgentCall)
import Agent.Bob.UserCommand.ConfigShow (ConfigShow)
import Agent.Bob.UserCommand.DeactivateSkill (DeactivateSkill)
import Agent.Bob.UserCommand.Init (Init)
import Agent.Bob.UserCommand.Name (Name)
import Agent.Bob.UserCommand.PermissionsShow (PermissionsShow)
import Agent.Bob.UserCommand.Resume (Resume)
import Agent.Bob.UserCommand.Soul (Soul)
import Agent.Bob.UserCommand.StateShow (StateShow)

-- | An existential over a user command type (not a value).
data SomeUserCommand
  = forall c. (Aeson.ToJSON c, IsCommand c, IsEvent c, IsUserCommand c)
  => SomeUserCommand (Proxy c)

-- | Every user command except 'Agent.Bob.UserCommand.Help.Help', which
-- renders this list and so cannot appear in it without an import cycle.
baseUserCommands :: [SomeUserCommand]
baseUserCommands =
  [ SomeUserCommand $ Proxy @ConfigShow
  , SomeUserCommand $ Proxy @DeactivateSkill
  , SomeUserCommand $ Proxy @Init
  , SomeUserCommand $ Proxy @Name
  , SomeUserCommand $ Proxy @PermissionsShow
  , SomeUserCommand $ Proxy @Resume
  , SomeUserCommand $ Proxy @Soul
  , SomeUserCommand $ Proxy @StateShow
  -- Must be second last:
  , SomeUserCommand $ Proxy @ActivateSkill
  -- Must be last:
  , SomeUserCommand $ Proxy @AgentCall
  ]
