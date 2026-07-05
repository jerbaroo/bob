module Agent.Bob.Prelude.Typeable where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import Witch (from)
import Prelude

-- | The name of a type, as rendered by its 'TypeRep' — e.g. @showTyCon \@Int@
-- is @"Int"@. Used to name events in the session log and to label tests.
showTyCon :: forall a. Typeable a => Text
showTyCon = from $ show $ typeRep (Proxy @a)
