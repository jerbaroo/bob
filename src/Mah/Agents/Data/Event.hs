module Mah.Agents.Data.Event where

import Mah.Agents.Data.AgentTask (AgentTask)
import Witch (From(..))

data Event = forall e. IsEvent e => Event e

instance IsEvent Event where
  eventTasks (Event e) = eventTasks e

-- | Something that has happened.
class IsEvent e where
  eventTasks :: e -> Maybe [AgentTask]
  eventTasks _ = Nothing

instance IsEvent e => From e Event where
  from = Event
