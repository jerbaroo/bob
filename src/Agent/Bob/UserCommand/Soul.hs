module Agent.Bob.UserCommand.Soul where

import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Config (ConfigF(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Effect.File (readFile)
import Agent.Bob.Prelude hiding (readFile)
import Agent.Bob.UserCommand (IsUserCommand(..))
import Data.Aeson qualified as Aeson

-- | Print the model's soul to the console: @/soul@.
data Soul = Soul

instance Aeson.ToJSON Soul where
  toJSON Soul = "Soul"

instance IsCommand Soul where
  type Result Soul = ()
  runCommand _agentId Soul = do
    soulPath <- asks (.soulPath)
    message <- case soulPath of
      Nothing   -> pure "No soul configured."
      Just path -> maybe
        ("No soul found at " <> path <> ".")
        (\soul -> "Soul (" <> path <> "):\n\n" <> soul)
        <$> readFile path
    writeSection System Info message
    pure ((), [])

instance IsEvent Soul where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand Soul where
  mkUserCommand [] = Just Soul
  mkUserCommand _ = Nothing
  userCommandDescription = "Print the model's soul."
  userCommandName = ["/soul"]
