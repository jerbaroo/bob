module Agent.Bob.Data.Event.Registry where

import Agent.Bob.Data.AgentOutput (AgentOutput)
import Agent.Bob.Data.Event (Event, IsEvent, eventName)
import Agent.Bob.Data.ToolResult (ToolResult)
import Agent.Bob.Data.UserInput (UserInput)
import Agent.Bob.Prelude
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson

-- | An existential over an event type (not a value). The 'Arbitrary', 'Eq' and
-- 'Show' constraints aren't needed to parse events, but let the test suite
-- derive round-trip property tests by iterating over 'knownEvents'.
data SomeEvent
  = forall e.
    ( Aeson.FromJSON e, Aeson.ToJSON e, IsEvent e, Typeable e
    , Arbitrary e, Eq e, Show e
    )
  => SomeEvent (Proxy e)

-- | Every event that can be restored from a persisted session log. Other events
-- (e.g. commands) are currently absent: they carry no context worth restoring
-- for the model. TODO restore the entire history.
knownEvents :: [SomeEvent]
knownEvents =
  [ SomeEvent $ Proxy @AgentOutput
  , SomeEvent $ Proxy @ToolResult
  , SomeEvent $ Proxy @UserInput
  ]

-- | The three ways a persisted log line can be interpreted on restore.
data LineResult
  = Restored Event  -- ^ A known event type, parsed successfully.
  | Skipped         -- ^ Well-formed envelope whose type isn't restored (e.g. a
                    --   command). Expected and harmless — not an error.
  | Malformed       -- ^ Not a valid event line, or a known type that failed to
                    --   decode. Worth reporting.

instance From LineResult (Maybe Event) where
  from (Restored event) = Just event
  from Skipped = Nothing
  from Malformed = Nothing

readEventLine :: Text -> LineResult
readEventLine line =
  case Aeson.parseMaybe eventPair =<< Aeson.decodeStrictText line of
    Nothing -> Malformed
    Just (name, value)
      | not (any (nameMatches name) knownEvents) -> Skipped
      | otherwise -> maybe Malformed Restored $ firstJust (parse name value) knownEvents
 where
  eventPair = Aeson.withObject "Event" \o -> (,) <$> o .: "name" <*> o .: "value"

  nameMatches :: Text -> SomeEvent -> Bool
  nameMatches name (SomeEvent (_ :: Proxy e)) = from (eventName @e) == name

  parse :: Text -> Aeson.Value -> SomeEvent -> Maybe Event
  parse name value (SomeEvent (_ :: Proxy e))
    | from (eventName @e) == name =
        from <$> Aeson.parseMaybe (Aeson.parseJSON @e) value
    | otherwise = Nothing
