{-# OPTIONS_GHC -Wno-orphans #-}

module Mah.Agents.Data.Event where

import Mah.Agents.Data.Task (Task)
import Mah.Agents.Data.Message (Message)
import Witch (From(..), TryFrom(..), maybeTryFrom)

data Event = forall e. IsEvent e => Event e

instance IsEvent Event where
  eventTasks (Event e) = eventTasks e

-- | Something that has happened.
class IsEvent e where
  eventTasks :: e -> Maybe [Task]
  eventTasks _ = Nothing
  eventToMessage :: e -> Maybe Message
  eventToMessage _ = Nothing

instance IsEvent e => From e Event where
  from = Event

instance IsEvent e => TryFrom e Message where
  tryFrom = maybeTryFrom eventToMessage

instance IsEvent e => TryFrom e [Task] where
  tryFrom = maybeTryFrom eventTasks
