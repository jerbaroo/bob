module Agent.Bob.Prompt.Build where

import Agent.Bob.Data.AgentInput (AgentInput(..))
import Agent.Bob.Data.Event (Event, eventToMessage)
import Agent.Bob.Data.Message (Message(..))
import Agent.Bob.Data.Role (Role(..))
import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))
import Agent.Bob.Prompt (ToPrompt(..))
import Agent.Bob.Tool.Registry (toolCallSchema, toolDescriptions)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)

buildPrompt :: AgentInput -> Aeson.Value
buildPrompt input@AgentInput{..} =
  Aeson.object
    [ ("messages", Aeson.toJSON $ systemMessage : eventMessages events)
    , ("model", Aeson.toJSON modelName)
    , ("response_format", Aeson.object
        [ ("type", "json_schema")
        , ("schema", responseSchema)
        ])
    , ("temperature", Aeson.toJSON modelTemperature)
    ]
 where
  systemMessage :: Message
  systemMessage = Message
    { role = System
    , thinking = Nothing
    , content = Text.intercalate "\n\n" $
        filter (not . Text.null) $ snd <$> promptSections input
    }

-- | Events are stored newest-first; reverse into chronological order. The
-- current user input and any tool results are among them ('UserInput' and
-- 'ToolResult' are recorded before the agent is called), so no separate
-- messages are added.
eventMessages :: [Event] -> [Message]
eventMessages = reverse . catMaybes . fmap eventToMessage

-- | The labelled components of the system message, in prompt order. The
-- single source of truth shared by 'buildPrompt' and 'contextBreakdown', so
-- a section added here shows up in both. Empty sections are dropped from
-- the prompt (and from the breakdown).
promptSections :: AgentInput -> [(Text, Text)]
promptSections AgentInput{..} =
  [ ("persona", personaStr)
  , ("environment", environmentStr)
  , ("soul", modelSoul)
  , ("active_skills", activeSkillsStr)
  , ("tasks", tasksStr)
  , ("response_format", responseFormatStr)
  , ("tools", availableToolsStr)
  , ("skills", availableSkillsStr)
  ]
 where
  personaStr = "You are an intelligent assistant responsible for tracking tasks and conversing with the user. The task list is your working memory: use it to keep structure about what you are doing and plan to do."
  environmentStr = Text.intercalate "\n"
    [ "### ENVIRONMENT"
    , "- model: " <> modelName
    , "- current_time: "
        <> from (formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" currentTime)
    , "- project_root: " <> projectRoot
    , "- git_branch: " <> gitBranch
    , "- os: " <> operatingSystem
    ]
  tasksStr = Text.intercalate "\n" $ "### PENDING TASKS" : case modelTasks of
    [] -> ["None."]
    _  ->
      [ "Consider the user's input and decide which of these remain:"
      , toPrompt modelTasks
      ]
  responseFormatStr = Text.intercalate "\n"
    [ "### RESPONSE FORMAT"
    , "Your response is a single JSON object (its shape is enforced for you). Fill"
    , "in its keys as follows:"
    , "- \"message\" (required): a string containing your conversational reply to the"
    , "  user. Markdown IS allowed inside this string (code fences, lists, emphasis)."
    , "- \"remaining_tasks\" (required): an array of strings: your up-to-date version of"
    , "  PENDING TASKS above. Maintain it actively: ADD new tasks (whether requested by"
    , "  the user or that you identify as needed to make progress), KEEP tasks that are"
    , "  still pending, and REMOVE tasks that are now complete or no longer relevant."
    , "  Use an empty array [] when nothing remains."
    , "- \"tool_calls\" (optional): an array of tool requests, each of the form"
    , "  {\"name\": \"<ToolName>\", \"arguments\": {...}}. Include it ONLY when you need a"
    , "  tool to make progress; you will be called again with the results as the next"
    , "  input. Only the tools listed under AVAILABLE TOOLS below may be requested."
    , ""
    , "Example of a valid response:"
    , "{\"remaining_tasks\": [\"Buy milk\", \"Call the dentist\"], \"message\": \"I've added those two tasks for you.\"}"
    ]
  availableToolsStr = Text.intercalate "\n"
    $ "### AVAILABLE TOOLS" : toolDescriptions
  availableSkillsStr = Text.intercalate "\n" $ "### AVAILABLE SKILLS" :
    case modelSkills of
      [] -> ["None."]
      _  ->
        [ "To use a skill, read its file with the read_files tool."
        , toPrompt modelSkills
        ]
  -- User-activated skills: their full content is included so the model
  -- can follow them without a tool call.
  activeSkillsStr = case modelSkillsActive of
    [] -> ""
    _  -> Text.intercalate "\n\n" $
      [ "### ACTIVE SKILLS"
      , "The user activated these skills. You should use them; their full instructions follow."
      ] <> [ "#### " <> name <> "\n\n" <> body | (name, body) <- modelSkillsActive ]

-- | Exact character count of each part of the prompt that 'buildPrompt'
-- would send, in prompt order. Only the derived token figure is an
-- estimate (~4 chars/token; exact token counts are only known from the
-- engine's response). Empty sections are omitted; the events entry counts
-- the message contents (and any thinking) that would be replayed.
contextBreakdown :: AgentInput -> [(Text, Int)]
contextBreakdown input =
     [ (label, Text.length content)
     | (label, content) <- promptSections input, not (Text.null content) ]
  -- "session_history" not "events": the latter is internal terminology,
  -- while user-facing output speaks of the session history.
  <> [("session_history", sum $ messageSize <$> eventMessages input.events)]
 where
  messageSize Message{..} =
    Text.length content + maybe 0 (Text.length . into @Text) thinking

-- | 'contextBreakdown' rendered for the console: total up front, then the
-- parts largest-first with their share of the total. Counts and percentages
-- are right-justified so the columns align.
renderContextBreakdown :: AgentInput -> Text
renderContextBreakdown input =
  let breakdown = sortOn (negate . snd) $ contextBreakdown input
      total = sum $ snd <$> breakdown
      percent n
        | total == 0 = 0 :: Int
        | otherwise = round @Double $ 100 * fromIntegral n / fromIntegral total
      width f = maximum $ Text.length . f <$> breakdown
      countWidth = width $ pretty . snd
      percentWidth = width $ pretty . percent . snd
      justify w = Text.justifyRight w ' '
      row (label, n) = (label,
           justify countWidth (pretty n)
        <> " (" <> justify percentWidth (pretty $ percent n) <> "%)")
  in     "Context: " <> pretty total <> " chars (~"
      <> pretty (total `div` 4) <> " tokens)\n" <> pretty (row <$> breakdown)

-- | JSON Schema for the model's response.
responseSchema :: Aeson.Value
responseSchema = Aeson.object
  [ ("type", "object")
  , ("additionalProperties", Aeson.Bool False)
  , ("required", Aeson.toJSON ["message", "remaining_tasks" :: Text])
  , ("properties", Aeson.object
      [ ("message", Aeson.object [("type", "string")])
      , ("remaining_tasks", Aeson.object
          [ ("type", "array")
          , ("items", Aeson.object [("type", "string")])
          ])
      , ("tool_calls", Aeson.object
          [ ("type", "array")
          , ("items", toolCallSchema)
          ])
      ])
  ]
