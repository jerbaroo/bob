module Agent.Bob.UserCommand.Help where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Event (IsEvent(..), KnownEvent)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (pretty)
import Agent.Bob.UserCommand (IsUserCommand(..))
import Agent.Bob.UserCommand.Registry.Base (SomeUserCommand(..), baseUserCommands)

-- | Print a short description of every user command: @/help@.
data Help = Help

instance Aeson.ToJSON Help where
  toJSON Help = "Help"

instance IsCommand Help where
  type Result Help = ()
  runCommand _agentId Help = do
    writeSection System Info $ pretty helpEntries
    pure ((), [])

instance KnownEvent Help

instance IsEvent Help where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand Help where
  mkUserCommand [] = Just Help
  mkUserCommand _ = Nothing
  userCommandDescription = "Print a short description of every user command."
  userCommandName = ["/help"]

-- | Name and description of every user command, alphabetical by name. 'Help'
-- itself cannot appear in 'baseUserCommands' (due to an import cycle), so it is
-- added here.
helpEntries :: [(Text, Text)]
helpEntries =
    sortOn fst
  $ filter (not . Text.null . fst) -- Don't include agent-call command.
  $ fmap (first Text.unwords)
  $ helpEntry : (entry <$> baseUserCommands)
 where
  entry (SomeUserCommand (_ :: Proxy c)) =
    (userCommandName @c, userCommandDescription @c)
  helpEntry = (userCommandName @Help, userCommandDescription @Help)
