module Urist.UndergroundRegion where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id
import qualified Data.Set as S

newtype UndergroundRegionId = UndergroundRegionId Int
  deriving newtype (Show, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype UndergroundRegionDepth = UndergroundRegionDepth Int
  deriving newtype (Show, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

data UndergroundRegionType = Cavern | Underworld | Magma
  deriving stock (Show, Read, Ord, Eq, Bounded, Enum, Generic)

data UndergroundRegion = UndergroundRegion
  { undergroundRegionId :: UndergroundRegionId
  , depth :: UndergroundRegionDepth
  , undergroundRegionType :: UndergroundRegionType
  , coords :: S.Set Coord
  } deriving stock (Show)

parseUndergroundRegion :: (Error Text :> es, State NodeMap :> es) => Eff es UndergroundRegion
parseUndergroundRegion = do
  undergroundRegionId <- takeId UndergroundRegionId
  depth <- UndergroundRegionDepth <$> takeNodeAsInt "depth"
  undergroundRegionType <- takeNodeAsReadable "type"
  coords <- asCoordList =<< takeNodeAsText "coords"
  pure $ UndergroundRegion { undergroundRegionId, depth, undergroundRegionType, coords }