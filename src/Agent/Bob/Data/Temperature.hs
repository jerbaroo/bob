module Agent.Bob.Data.Temperature where

import Agent.Bob.Prelude
import Agent.Bob.Pretty (Pretty(..))
import Data.Aeson as Aeson

newtype Temperature = Temperature Float
  deriving newtype (Fractional, Num)

instance Aeson.FromJSON Temperature where
  parseJSON = fmap Temperature . Aeson.parseJSON

instance Aeson.ToJSON Temperature where
  toJSON (Temperature n) = Aeson.toJSON n

instance Pretty Temperature where
  pretty (Temperature n) = from (show n)
