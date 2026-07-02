module Mah.Agents.Data.Temperature where

import Mah.Agents.Pretty (Pretty(..))
import Witch (from)

newtype Temperature = Temperature Float
  deriving newtype (Fractional, Num)

instance Pretty Temperature where
  pretty (Temperature n) = from (show n)
