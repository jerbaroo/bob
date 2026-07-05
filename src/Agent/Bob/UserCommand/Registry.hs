module Agent.Bob.UserCommand.Registry where

import Agent.Bob.Command (Command(..))
import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Prelude
import Agent.Bob.UserCommand (IsUserCommand(..))
import Agent.Bob.UserCommand.Help (Help)
import Agent.Bob.UserCommand.Registry.Base (SomeUserCommand(..), baseUserCommands)

-- | Every user command the harness knows how to run, tried in order:
-- 'AgentCall' parses any input, so it must be last.
knownUserCommands :: [SomeUserCommand]
knownUserCommands = SomeUserCommand (Proxy @Help) : baseUserCommands

-- | The first of 'knownUserCommands' to parse the input, wrapped as a harness
-- 'Command' ready to execute. Never 'Nothing' in practice: 'AgentCall' parses
-- any input.
parseCommand :: UserInput -> Maybe Command
parseCommand input = firstJust parse knownUserCommands
 where
  parse (SomeUserCommand (_ :: Proxy c)) = Command <$> parseUserCommand @c input
