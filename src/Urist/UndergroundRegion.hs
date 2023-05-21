module Urist.UndergroundRegion where

import Solitude
import Effectful.Error.Static
import Data.Char
import qualified Xeno.DOM as XML
import Urist.ParseHelpers

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
  } deriving stock (Show)

parseUndergroundRegion :: (Error Text :> es) => Map ByteString XML.Node -> Eff es UndergroundRegion
parseUndergroundRegion m = do
  expectOnly ["id", "depth", "type"] m
  undergroundRegionId <- parseId UndergroundRegionId m
  depth <- UndergroundRegionDepth <$> getNodeInt "depth" m
  undergroundRegionType <- parseType "underground region" m
  pure $ UndergroundRegion { undergroundRegionId, depth, undergroundRegionType }