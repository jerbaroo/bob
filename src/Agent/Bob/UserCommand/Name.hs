module Agent.Bob.UserCommand.Name where

import Data.Aeson qualified as Aeson
import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Config (ConfigF(..))
import Agent.Bob.Data.Event (IsEvent(..), KnownEvent)
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Effect.File (readFileMaybe, renameFile)
import Agent.Bob.Prelude
import Agent.Bob.Session (sanitise)
import Agent.Bob.UserCommand (IsUserCommand(..))
import Data.Maybe (isJust)

-- | Rename the current session: @/name <new-name>@. Updates 'sessionPath'
-- and moves the history file when it exists (it may not, before any events).
newtype Name = Name Text

instance Aeson.ToJSON Name where
  toJSON (Name name) = Aeson.object [("session_name", Aeson.toJSON name)]

instance IsCommand Name where
  type Result Name = ()
  runCommand _agentId (Name raw) = do
    memoryPath <- asks (.memoryPath)
    let target = memoryPath <> "/" <> sanitise raw
    taken <- isJust <$> readFileMaybe target
    if taken
      then writeSection System Error $ "Session already exists: " <> target
      else do
        current <- gets (.sessionPath)
        onDisk <- isJust <$> readFileMaybe current
        when onDisk $ renameFile current target
        modify \s -> s{sessionPath = target}
        writeSection System Info $ "Session renamed to: " <> target
    pure ((), [])

instance KnownEvent Name

instance IsEvent Name where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand Name where
  mkUserCommand [name] = Just $ Name name
  mkUserCommand _ = Nothing
  userCommandDescription = "Rename the current session."
  userCommandName = ["/name"]
