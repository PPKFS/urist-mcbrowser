module Urist.HistoricalEra where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data HistoricalEra = HistoricalEra
  { name :: Text
  , startYear :: Int
  , endYear :: Maybe Int
  }

parseHistoricalEra :: (Error Text :> es, State NodeMap :> es) => Eff es HistoricalEra
parseHistoricalEra = do
  name <- takeNodeAsText "name"
  startYear <- takeNodeAsInt "start_year"
  endYear <- takeNodeMaybeAsInt "end_year"
  pure $ HistoricalEra { name, startYear, endYear }