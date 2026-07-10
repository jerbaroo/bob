module Agent.Bob.UserCommand.StateShow where

import Data.Aeson qualified as Aeson
import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Config (ConfigF(..))
import Agent.Bob.Data.Event (IsEvent(..), KnownEvent)
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.UserCommand (IsUserCommand(..))
import Data.Map qualified as Map
import Data.Text qualified as Text

-- | Print a summary of the current harness state: @/state@. Deliberately a
-- summary, not a full dump: the session history is already visible in the
-- transcript (and its size in the context breakdown).
data StateShow = StateShow

instance Aeson.ToJSON StateShow where
  toJSON StateShow = "StateShow"

instance IsCommand StateShow where
  type Result StateShow = ()
  runCommand agentId StateShow = do
    Config{soulPath} <- ask
    HarnessState{..} <- get
    let entries = maybe 0 length $ Map.lookup agentId events
        currentTasks = fromMaybe [] $ Map.lookup agentId tasks
        pairs :: [(Text, Text)]
        pairs =
          [ ("activated_skills", case userActivatedSkills of
              [] -> "(none)"
              _  -> Text.intercalate ", " userActivatedSkills)
          , ("model_temperature", pretty modelTemperature)
          , ("session", sessionPath)
          , ("session_history", counted entries "entry" "entries")
          , ("skills_found", pretty $ length skills)
          , ("soul_source", fromMaybe "(built-in default)" soulPath)
          , ("tasks", pretty $ length currentTasks)
          ]
    writeSection System Info $ "Harness state:\n" <> pretty pairs
      <> case currentTasks of
           [] -> ""
           _  -> "\n\nTasks:\n" <> pretty currentTasks
    pure ((), [])

-- | @counted 1 "entry" "entries" == "1 entry"@.
counted :: Int -> Text -> Text -> Text
counted 1 singular _ = "1 " <> singular
counted n _ plural = pretty n <> " " <> plural

instance KnownEvent StateShow

instance IsEvent StateShow where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand StateShow where
  mkUserCommand [] = Just StateShow
  mkUserCommand _ = Nothing
  userCommandDescription = "Print a summary of the current harness state."
  userCommandName = ["/state"]
