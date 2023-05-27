module Urist.HistoricalEvent where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data HistoricalEvent = HistoricalEvent

parseHistoricalEvent :: (Error Text :> es, State NodeMap :> es) => Eff es HistoricalEvent
parseHistoricalEvent = pure HistoricalEvent