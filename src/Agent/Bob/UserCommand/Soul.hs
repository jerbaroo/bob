module Agent.Bob.UserCommand.Soul where

import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Config (ConfigF(..))
import Agent.Bob.Data.Event (IsEvent(..), KnownEvent)
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Effect.File (readFileMaybe)
import Agent.Bob.Prelude
import Agent.Bob.Soul (getSoul)
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
    -- Read the file only to report where the soul came from; 'getSoul' is
    -- the source of truth for the text itself (and its change detection).
    fromFile <- maybe (pure Nothing) readFileMaybe soulPath
    soul <- getSoul
    let header = case (soulPath, fromFile) of
          (Just path, Just _)  -> "Soul (" <> path <> "):"
          (Just path, Nothing) -> "Default soul (no file at " <> path <> "):"
          (Nothing, _)         -> "Default soul (no soul_path configured):"
    writeSection System Info $ header <> "\n\n" <> soul
    pure ((), [])

instance KnownEvent Soul

instance IsEvent Soul where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand Soul where
  mkUserCommand [] = Just Soul
  mkUserCommand _ = Nothing
  userCommandDescription = "Print the model's soul."
  userCommandName = ["/soul"]
