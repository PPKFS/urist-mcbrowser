module Urist.HistoricalEventCollection where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data HistoricalEventCollection = HistoricalEventCollection

parseHistoricalEventCollection :: (Error Text :> es, State NodeMap :> es) => Eff es HistoricalEventCollection
parseHistoricalEventCollection = pure HistoricalEventCollection