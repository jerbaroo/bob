module Agent.Bob.UserCommand.DeactivateSkill where

import Data.Aeson qualified as Aeson
import Agent.Bob.Command (IsCommand(..))
import Agent.Bob.Data.Event (IsEvent(..), KnownEvent)
import Agent.Bob.Data.HarnessState (HarnessState(..))
import Agent.Bob.Data.Level (Level(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Effect.Console (writeSection)
import Agent.Bob.Prelude
import Agent.Bob.UserCommand (IsUserCommand(..))
import Data.List (delete)

-- | Deactivate a previously activated skill: @/deactivate <name>@. The
-- inverse of 'Agent.Bob.UserCommand.ActivateSkill.ActivateSkill': the
-- skill's instructions leave the prompt (it stays available for the model
-- to read on demand).
newtype DeactivateSkill = DeactivateSkill Text

instance Aeson.ToJSON DeactivateSkill where
  toJSON (DeactivateSkill name) =
    Aeson.object [("deactivate_skill", Aeson.toJSON name)]

instance IsCommand DeactivateSkill where
  type Result DeactivateSkill = ()
  runCommand _agentId (DeactivateSkill name) = do
    active <- gets (.userActivatedSkills)
    if name `elem` active
      then do
        modify \s -> s{userActivatedSkills = delete name s.userActivatedSkills}
        writeSection System Info $ "Deactivated skill: " <> name
      else writeSection System Error $ "Skill not active: " <> name
    pure ((), [])

instance KnownEvent DeactivateSkill

instance IsEvent DeactivateSkill where
  eventTasks _ = Nothing
  eventToMessage _ = Nothing

instance IsUserCommand DeactivateSkill where
  mkUserCommand [name] = Just $ DeactivateSkill name
  mkUserCommand _ = Nothing
  userCommandDescription =
    "Deactivate a skill (e.g. /deactivate caveman): its instructions leave the prompt."
  userCommandName = ["/deactivate"]
