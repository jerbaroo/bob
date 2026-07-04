module Mah.Agents.Data.Event.Registry where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.List.Extra (firstJust)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import Mah.Agents.Data.AgentOutput (AgentOutput)
import Mah.Agents.Data.Event (Event, IsEvent)
import Mah.Agents.Data.UserInput (UserInput)
import Witch (from)

-- | An existential over an event type (not a value).
data SomeEvent
  = forall e. (Aeson.FromJSON e, Aeson.ToJSON e, IsEvent e, Typeable e)
  => SomeEvent (Proxy e)

-- | Every event that can be restored from a persisted session log. Other events
-- (e.g. commands) are currently absent: they carry no context worth restoring
-- for the model. TODO restore the entire history.
knownEvents :: [SomeEvent]
knownEvents =
  [ SomeEvent $ Proxy @AgentOutput
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
    | from (show (typeRep (Proxy @e))) == name =
        from <$> Aeson.parseMaybe (Aeson.parseJSON @e) value
    | otherwise = Nothing
