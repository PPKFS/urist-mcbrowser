module Urist.Landmass where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id


data Landmass = Landmass
  { landmassId :: LandmassId
  , name :: Text
  , coord1 :: Coord
  , coord2 :: Coord

  }

parseLandmass :: (Error Text :> es, State NodeMap :> es) => Eff es Landmass
parseLandmass = do
  landmassId <- LandmassId <$> takeNodeAsInt "id"
  name <- takeNodeAsText "name"
  coord1 <- asCoord =<< takeNodeAsText "coord_1"
  coord2 <- asCoord =<< takeNodeAsText "coord_2"
  pure $ Landmass { landmassId, name, coord1, coord2 }
