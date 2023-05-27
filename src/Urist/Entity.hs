module Urist.Entity where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data Entity = Entity

parseEntity :: (Error Text :> es, State NodeMap :> es) => Eff es Entity
parseEntity = pure Entity