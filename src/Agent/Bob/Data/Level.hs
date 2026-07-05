module Agent.Bob.Data.Level where

import Agent.Bob.Prelude

import Agent.Bob.Pretty (Pretty(..))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

data Level
  = Info
  | Thinking
  | Meta
  | Debug
  deriving (Eq, Ord, Show)

instance From Level Text where
  from = Text.toLower . from . show

instance Pretty Level where
  pretty = into @Text

instance Aeson.FromJSON Level where
  parseJSON = Aeson.withText "Level" $ either (fail . from) pure . parseLevel

allLevels :: [Level]
allLevels = [Debug, Info, Meta, Thinking]

parseLevel :: Text -> Either Text Level
parseLevel x = case filter ((== x) . into @Text) allLevels of
  [level] -> Right level
  _ -> Left $
       "Unknown Level: "
    <> x
    <> " (want one of: "
    <> Text.intercalate "/" (into @Text <$> allLevels)
    <> ")"
