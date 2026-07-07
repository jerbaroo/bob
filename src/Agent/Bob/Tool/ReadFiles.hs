module Agent.Bob.Tool.ReadFiles where

import Agent.Bob.Effect.File (readFileMaybe, resolvePath)
import Agent.Bob.Glob (Glob(..))
import Agent.Bob.Glob qualified as Glob
import Agent.Bob.Prelude
import Agent.Bob.Tool (IsTool(..))
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

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
  -- A grant for this invocation covers future reads in the same directory.
  permissionParam ReadFiles{..} = directory <> "/*"
  renderOutput _ = Text.intercalate "\n\n" . fmap render
   where
    render (path, Just c)  = "== " <> path <> " ==\n" <> c
    render (path, Nothing) = "== " <> path <> " == (could not read)"
  runTool ReadFiles{..} =
    forM files \f -> (f,) <$> readFileMaybe (directory <> "/" <> f)
  -- Mirrors 'Aeson.FromJSON': both fields are required.
  toolArgsSchema = Aeson.object
    [ ("type", "object")
    , ("additionalProperties", Aeson.Bool False)
    , ("required", Aeson.toJSON ["directory", "files" :: Text])
    , ("properties", Aeson.object
        [ ("directory", Aeson.object [("type", "string")])
        , ("files", Aeson.object
            [ ("type", "array")
            , ("items", Aeson.object [("type", "string")])
            ])
        ])
    ]
  toolDescription = Text.unlines
    [ "- ReadFiles: read the contents of one or more files within a directory."
    , "  Arguments: {\"directory\": \"<path>\", \"files\": [\"<name>\", ...]}"
    , "  Example: {\"name\": \"ReadFiles\", \"arguments\": {\"directory\": \"src\", \"files\": [\"Main.hs\"]}}"
    ]
