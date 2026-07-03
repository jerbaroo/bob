module Mah.Agents.Data.Level where

import Mah.Agents.Pretty (Pretty(..))

data Level
  = Info
  | Thinking
  | Debug
  deriving (Eq, Ord)

instance Pretty Level where
  pretty Debug = "debug"
  pretty Info = "info"
  pretty Thinking = "thinking"
