module Urist.Region where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id
import qualified Data.Set as S

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

data Evilness =
    Good
    | Neutral
    | Evil
  deriving stock (Show, Read, Ord, Eq, Bounded, Enum, Generic)

data Region = Region
  { regionId :: RegionId
  , name :: Text
  , regionType :: RegionType
  , forceId :: Maybe ForceId
  , evilness :: Evilness
  , coords :: S.Set Coord
  } deriving stock (Show)

parseRegion :: (Error Text :> es, State NodeMap :> es) => Eff es Region
parseRegion = do
  regionId <- takeId RegionId
  name <- takeNodeAsText "name"
  regionType <- takeNodeAsReadable "type"
  forceId <- ForceId <$$> takeNodeMaybeAsInt "force_id"
  evilness <- takeNodeAsReadable "evilness"
  coords <- asCoordList =<< takeNodeText "coords"
  pure $ Region { regionId, name, regionType, forceId, evilness, coords }