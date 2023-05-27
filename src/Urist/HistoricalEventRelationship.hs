module Urist.HistoricalEventRelationship where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data HistoricalEventRelationship = HistoricalEventRelationship

parseHistoricalEventRelationship :: (Error Text :> es, State NodeMap :> es) => Eff es HistoricalEventRelationship
parseHistoricalEventRelationship = pure HistoricalEventRelationship