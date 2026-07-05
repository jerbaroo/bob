module Agent.Bob.Data.Event.Registry where

import Agent.Bob.Data.AgentOutput (AgentOutput)
import Agent.Bob.Data.Event (Event, IsEvent)
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

-- | Parse one line of a persisted session log back into an 'Event'.
parseEventLine :: Text -> Maybe Event
parseEventLine line = do
  (name, value) <- Aeson.parseMaybe eventPair =<< Aeson.decodeStrictText line
  firstJust (parse name value) knownEvents
 where
  eventPair = Aeson.withObject "Event" \o -> (,) <$> o .: "name" <*> o .: "value"
  parse :: Text -> Aeson.Value -> SomeEvent -> Maybe Event
  parse name value (SomeEvent (_ :: Proxy e))
    | from (showTyCon @e) == name =
        from <$> Aeson.parseMaybe (Aeson.parseJSON @e) value
    | otherwise = Nothing
