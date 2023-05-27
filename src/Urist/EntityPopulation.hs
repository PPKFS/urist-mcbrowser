module Urist.EntityPopulation where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data EntityPopulation = EntityPopulation

parseEntityPopulation :: (Error Text :> es, State NodeMap :> es) => Eff es EntityPopulation
parseEntityPopulation = pure EntityPopulation