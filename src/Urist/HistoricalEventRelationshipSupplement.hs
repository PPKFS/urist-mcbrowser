module Urist.HistoricalEventRelationshipSupplement where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data HistoricalEventRelationshipSupplement = HistoricalEventRelationshipSupplement

parseHistoricalEventRelationshipSupplement :: (Error Text :> es, State NodeMap :> es) => Eff es HistoricalEventRelationshipSupplement
parseHistoricalEventRelationshipSupplement = pure HistoricalEventRelationshipSupplement