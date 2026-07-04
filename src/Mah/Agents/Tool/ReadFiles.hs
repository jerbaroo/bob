module Mah.Agents.Tool.ReadFiles where

import Control.Monad (forM)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (readFile)
import Mah.Agents.Effect.File (readFile, resolvePath)
import Mah.Agents.Glob (Glob(..))
import Mah.Agents.Glob qualified as Glob
import Mah.Agents.Tool (IsTool(..))

-- | Read one or more files within a directory.
data ReadFiles = ReadFiles
  { directory :: Text
  , files :: [Text]
  } deriving Show

instance Aeson.FromJSON ReadFiles where
  parseJSON = Aeson.withObject "ReadFiles" \o -> do
    directory <- o .: "directory"
    files     <- o .: "files"
    pure ReadFiles{..}

instance Aeson.ToJSON ReadFiles where
  toJSON ReadFiles{..} = Aeson.object
    [ ("directory", Aeson.toJSON directory)
    , ("files", Aeson.toJSON files)
    ]

instance IsTool ReadFiles where
  type ToolOutput ReadFiles = [(Text, Maybe Text)]
  -- Every file this invocation reads must fall under the granted glob.
  permissionGranted ReadFiles{..} param = do
    glob <- Glob <$> resolvePath param
    and <$> traverse
      (\f -> Glob.matches glob <$> resolvePath (directory <> "/" <> f))
      files
  renderOutput _ = Text.intercalate "\n\n" . fmap render
   where
    render (path, Just c)  = "== " <> path <> " ==\n" <> c
    render (path, Nothing) = "== " <> path <> " == (could not read)"
  runTool ReadFiles{..} =
    forM files \f -> (f,) <$> readFile (directory <> "/" <> f)
  toolDescription = Text.unlines
    [ "- ReadFiles: read the contents of one or more files within a directory."
    , "  Arguments: {\"directory\": \"<path>\", \"files\": [\"<name>\", ...]}"
    , "  Example: {\"name\": \"ReadFiles\", \"arguments\": {\"directory\": \"src\", \"files\": [\"Main.hs\"]}}"
    ]
