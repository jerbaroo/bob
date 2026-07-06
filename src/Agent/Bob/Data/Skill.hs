module Agent.Bob.Data.Skill where

import Agent.Bob.Prelude
import Agent.Bob.Prompt (ToPrompt(..))
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Frontmatter (IResult(..), parseYamlFrontmatter)
import Data.Text.Encoding (encodeUtf8)

-- | A skill the model can pull in on demand: a directory containing a
-- @SKILL.md@ whose YAML frontmatter gives a name and description.
data Skill = Skill
  { name        :: Text
  , description :: Text
  , path        :: Text  -- ^ Path to SKILL.md, for the model to read on demand.
  }

instance ToPrompt Skill where
  toPrompt Skill{..} = name <> " (" <> path <> "): " <> description

-- | The keys we read out of a SKILL.md's YAML frontmatter.
data SkillMeta = SkillMeta { name :: Text, description :: Text }

instance FromJSON SkillMeta where
  parseJSON = withObject "SkillMeta" \o ->
    SkillMeta <$> o .: "name" <*> o .: "description"

-- | Parse a SKILL.md into a 'Skill' via its YAML frontmatter.
parseSkill :: Text -> Text -> Maybe Skill
parseSkill path contents =
  case parseYamlFrontmatter $ encodeUtf8 contents of
    Done _ SkillMeta{name, description} -> Just Skill{name, description, path}
    _                                   -> Nothing
