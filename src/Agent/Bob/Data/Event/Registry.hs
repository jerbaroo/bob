module Agent.Bob.Data.Event.Registry where

import Agent.Bob.Data.AgentOutput (AgentOutput)
import Agent.Bob.Data.Event (Event, IsEvent, KnownEvent(..))
import Agent.Bob.Data.ToolResult (ToolResult)
import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Prelude
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson

-- | An existential over an event type (not a value).
data SomeEvent
  = forall e.
    ( Aeson.FromJSON e
    , Aeson.ToJSON e
    , Arbitrary e
    , Eq e
    , IsEvent e
    , KnownEvent e
    , Show e
    , Typeable e
    )
  => SomeEvent (Proxy e)

-- | Every event that we want to parse from a session log. Other events (e.g.
-- commands) are currently absent, they carry no context worth restoring.
eventsToParse :: [SomeEvent]
eventsToParse =
  [ SomeEvent $ Proxy @AgentOutput
  , SomeEvent $ Proxy @ToolResult
  , SomeEvent $ Proxy @UserInput
  ]

-- | The three ways a persisted log line can be interpreted on restore.
data LineResult = Malformed | Restored Event | Skipped

instance From LineResult (Maybe Event) where
  from (Restored event) = Just event
  from Skipped = Nothing
  from Malformed = Nothing

readEventLine :: Text -> LineResult
readEventLine line =
  case Aeson.parseMaybe eventPair =<< Aeson.decodeStrictText line of
    Nothing -> Malformed
    Just (name, value)
      | not (any (nameMatches name) eventsToParse) -> Skipped
      | otherwise -> maybe Malformed Restored $
          firstJust (parseEvent name value) eventsToParse
 where
  eventPair = Aeson.withObject "Event" \o -> do
    name <- o .: "name"
    value <- o .: "value"
    pure (name, value)

  nameMatches :: Text -> SomeEvent -> Bool
  nameMatches name (SomeEvent (_ :: Proxy e)) = from (eventName @e) == name

  parseEvent :: Text -> Aeson.Value -> SomeEvent -> Maybe Event
  parseEvent name value someEvent@(SomeEvent (_ :: Proxy e))
    | nameMatches name someEvent =
        from <$> Aeson.parseMaybe (Aeson.parseJSON @e) value
    | otherwise = Nothing
