module Agent.Bob.UserCommand.ActivateSkill where

import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Event (IsEvent(..))
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Data.Skill (Skill(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Prelude
import Agent.Bob.Skill (getSkills)
import Agent.Bob.UserCommand (IsUserCommand(..))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

-- | Activate a skill by name, typed as @/<name>@ (e.g. @/caveman@). An
-- activated skill's full SKILL.md is injected into every prompt, rather than
-- left for the model to fetch on demand. This is the fallback for any
-- single @/word@ not matched by another command, so it is registered just
-- before 'Agent.Bob.UserCommand.AgentCall.AgentCall'.
newtype ActivateSkill = ActivateSkill Text

instance Aeson.ToJSON ActivateSkill where
  toJSON (ActivateSkill name) =
    Aeson.object [("activate_skill", Aeson.toJSON name)]

instance IsCommand ActivateSkill where
  type Result ActivateSkill = ()
  runCommand _agentId (ActivateSkill name) = do
    skills <- getSkills
    if any ((== name) . (.name)) skills
      then do
        modify \s -> s
          { userActivatedSkills =
              if name `elem` s.userActivatedSkills
              then s.userActivatedSkills
              else name : s.userActivatedSkills
          }
        writeSection System Info $ "Activated skill: " <> name
      else writeSection System Error $ "No such skill: " <> name
    pure ((), [])

instance IsEvent ActivateSkill where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand ActivateSkill where
  userCommandName = ["/<skill>"]
  userCommandDescription =
    "Activate a skill by name (e.g. /caveman): its instructions are added to every prompt."
  -- A single '/word' becomes the skill name; anything else isn't ours.
  parseUserCommand input = case Text.words $ Text.strip $ from input of
    [word] | Just name <- Text.stripPrefix "/" word, not (Text.null name)
           -> Just $ ActivateSkill name
    _      -> Nothing
