module Urist.HistoricalEra where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data HistoricalEra = HistoricalEra

parseHistoricalEra :: (Error Text :> es, State NodeMap :> es) => Eff es HistoricalEra
parseHistoricalEra = pure HistoricalEra