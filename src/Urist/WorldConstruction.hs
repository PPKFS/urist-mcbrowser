module Urist.WorldConstruction where


import Solitude
import Effectful.Error.Static
import qualified Xeno.DOM as XML
import Urist.ParseHelpers
import Urist.Id

data WorldConstructionType =
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

data WorldConstruction = Region
  { worldConstructionId :: WorldConstructionId
  , name :: Text
  , worldConstructionType :: WorldConstructionType
  } deriving stock (Show)

parseWorldConstruction :: (Error Text :> es) => Map ByteString XML.Node -> Eff es WorldConstruction
parseWorldConstruction m = do
  expectOnly ["id", "name", "type"] m
  regionId <- parseId RegionId m
  name <- getNodeText "name" m
  --regionType <- parseType "region" m
  error ""--pure $ Region { regionId, name, regionType }