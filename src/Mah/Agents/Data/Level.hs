module Mah.Agents.Data.Level where

import Mah.Agents.Pretty (Pretty(..))

data Level
  = Info
  | Thinking
  | Meta
  | Debug
  deriving (Eq, Ord)

instance Pretty Level where
  pretty Debug = "debug"
  pretty Info = "info"
  pretty Meta = "meta"
  pretty Thinking = "thinking"
