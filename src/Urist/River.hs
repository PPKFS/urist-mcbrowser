module Urist.River where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data River = River

parseRiver :: (Error Text :> es, State NodeMap :> es) => Eff es River
parseRiver = pure River