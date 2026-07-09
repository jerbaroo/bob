module Agent.Bob.UserCommand.Registry where

import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Prelude
import Agent.Bob.UserCommand (IsUserCommand(..), UserCommand(..))
import Agent.Bob.UserCommand.Help (Help)
import Agent.Bob.UserCommand.Registry.Base (SomeUserCommand(..), baseUserCommands)

-- | Every @/<command>@ the harness knows how to run.
knownUserCommands :: [SomeUserCommand]
knownUserCommands = SomeUserCommand (Proxy @Help) : baseUserCommands

-- | The first of 'knownUserCommands' to parse the input. 'Nothing' when the
-- input is not a @/command@, i.e. it is input meant for the model.
parseUserCommand' :: UserInput -> Maybe UserCommand
parseUserCommand' input = flip firstJust knownUserCommands
  \(SomeUserCommand (_ :: Proxy c)) ->
    UserCommand <$> parseUserCommand @c input
