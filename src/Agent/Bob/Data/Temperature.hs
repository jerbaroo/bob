module Agent.Bob.Data.Temperature where

import Data.Aeson as Aeson
import Agent.Bob.Pretty (Pretty(..))
import Witch (from)

newtype Temperature = Temperature Float
  deriving newtype (Fractional, Num)

instance Aeson.ToJSON Temperature where
  toJSON (Temperature n) = Aeson.toJSON n

instance Pretty Temperature where
  pretty (Temperature n) = from (show n)
