module Agent.Bob.UserCommand.Resume where

import Data.Aeson qualified as Aeson
import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Config (ConfigF(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Effect.File (doesDirectoryExist, listDirectory)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.Session (restoreSession)
import Agent.Bob.UserCommand (IsUserCommand(..))
import Data.Ord (Down(..))
import Data.Text qualified as Text

-- | Switch to a previous session: @/resume <name>@. The argument may be any
-- unique fragment of the session name; the loaded history replaces the
-- current context.
newtype Resume = Resume Text

instance Aeson.ToJSON Resume where
  toJSON (Resume query) = Aeson.object [("resume", Aeson.toJSON query)]

instance IsCommand Resume where
  type Result Resume = ()
  runCommand agentId (Resume query) = do
    memoryPath <- asks (.memoryPath)
    current <- gets (.sessionPath)
    names <- doesDirectoryExist memoryPath >>= \case
      False -> pure []
      True  -> listDirectory memoryPath
    let path name = memoryPath <> "/" <> name
        candidates =
          filter (\n -> query `Text.isInfixOf` n && path n /= current) names
    case candidates of
      [name] -> do
        modify \s -> s{sessionPath = path name}
        restoreSession agentId $ path name
        writeSection System Info $ "Resumed session: " <> name
      [] -> writeSection System Error $ "No session matching: " <> query
      _  -> writeSection System Error $
        "Ambiguous, matches:\n" <> pretty (sortOn Down candidates)
    pure ((), [])

instance IsEvent Resume where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand Resume where
  mkUserCommand [query] = Just $ Resume query
  mkUserCommand _ = Nothing
  userCommandDescription =
    "Switch to a previous session by (a unique fragment of) its name."
  userCommandName = ["/resume"]
