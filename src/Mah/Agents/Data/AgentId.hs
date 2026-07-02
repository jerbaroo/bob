module Mah.Agents.Data.AgentId where

import Witch (From(..))
import Mah.Agents.Pretty (Pretty(..))

newtype AgentId = AgentId Int
  deriving (Eq, Ord, Show)

instance Pretty AgentId where
  pretty (AgentId n) = "Agent #" <> from (show n)
