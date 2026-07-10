module Agent.Bob.Data.Event where

import Agent.Bob.Prelude
import Data.Aeson qualified as Aeson
import Agent.Bob.Data.Task (Task)
import Agent.Bob.Data.Message (Message)
import Data.Time.Clock (UTCTime)

data Event = forall e. (Aeson.ToJSON e, IsEvent e, KnownEvent e) => Event e

instance (Aeson.ToJSON e, IsEvent e, KnownEvent e) => From e Event where
  from = Event

instance IsEvent Event where
  eventTasks (Event e) = eventTasks e
  eventToMessage (Event e) = eventToMessage e

instance Aeson.ToJSON Event where
  toJSON (Event (e :: e)) = Aeson.object
    [ ("name", Aeson.toJSON $ eventName @e)
    , ("value", Aeson.toJSON e)
    ]

-- | Something that has happened.
class IsEvent e where
  -- | Updated list of tasks for the harness state.
  eventTasks :: e -> Maybe [Task]
  -- | How the event appears in the model's context.
  eventToMessage :: e -> Maybe Message

-- | The name identifying an event type in the session log.
--
-- Type-directed so a name can be derived from a type alone. Kept apart from
-- 'IsEvent' so that 'Event' gets no instance.
class KnownEvent e where
  eventName :: Text
  default eventName :: Typeable e => Text
  eventName = from $ showTyCon @e

-- | The persisted form of an event: the JSON above plus when it happened and
-- a schema version, for forward-compatible parsing.
-- TODO use safe-json?
envelope :: UTCTime -> Event -> Aeson.Value
envelope time (Event (e :: e)) = Aeson.object
  [ ("name", Aeson.toJSON $ eventName @e)
  , ("time", Aeson.toJSON time)
  , ("value", Aeson.toJSON e)
  , ("version", Aeson.toJSON (1 :: Int))
  ]
