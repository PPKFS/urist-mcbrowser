module Urist.Region where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data RegionType =
    Hills
    | Forest
    | Grassland
    | Desert
    | Lake
    | Mountains
    | Wetland
    | Glacier
    | Tundra
    | Ocean
  deriving stock (Show, Read, Ord, Eq, Bounded, Enum, Generic)

data Region = Region
  { regionId :: RegionId
  , name :: Text
  , regionType :: RegionType
  } deriving stock (Show)

parseRegion :: (Error Text :> es, State NodeMap :> es) => Eff es Region
parseRegion = do
  regionId <- takeId RegionId
  name <- takeNodeAsText "name"
  regionType <- takeNodeAsReadable "type"
  pure $ Region { regionId, name, regionType }