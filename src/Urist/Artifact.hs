module Urist.Artifact where

import Solitude
import Effectful.Error.Static
import qualified Xeno.DOM as XML
import Urist.ParseHelpers
import Urist.Id

data RegionType = Hills | Forest | Grassland | Desert | Lake | Mountains | Wetland | Glacier | Tundra | Ocean
  deriving stock (Show, Read, Ord, Eq, Bounded, Enum, Generic)

type Item = ()
data Artifact = Artifact
  { regionId :: ArtifactId
  , name :: Text
  , item :: Item
  , siteId :: Maybe SiteId
  , structureLocalId :: Maybe LocalStructureId
  } deriving stock (Show)
{-
parseRegion :: (Error Text :> es) => Map ByteString XML.Node -> Eff es Region
parseRegion m = do
  expectOnly ["id", "name", "type"] m
  regionId <- parseId RegionId m
  name <- getNodeText "name" m
  regionType <- parseType "region" m
  pure $ Region { regionId, name, regionType }
-}