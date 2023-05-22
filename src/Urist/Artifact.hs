module Urist.Artifact where

import Solitude
import Effectful.Error.Static
import qualified Xeno.DOM as XML
import Urist.ParseHelpers
import Urist.Id

data RegionType = Hills | Forest | Grassland | Desert | Lake | Mountains | Wetland | Glacier | Tundra | Ocean
  deriving stock (Show, Read, Ord, Eq, Bounded, Enum, Generic)

data Item = Item
  { name :: Text
  , writtenContent :: Maybe ItemWrittenContent
  } deriving stock (Show)

data ItemWrittenContent = ItemWrittenContent
  { writtenContentId :: WrittenContentId
  , pageCount :: Maybe Int
  } deriving stock (Show)

data Artifact = Artifact
  { artifactId :: ArtifactId
  , name :: Text
  , item :: Item
  , siteId :: Maybe SiteId
  , structureLocalId :: Maybe LocalStructureId
  } deriving stock (Show)

parseArtifact :: (Error Text :> es) => Map ByteString XML.Node -> Eff es Artifact
parseArtifact m = do
  expectOnly ["id", "name", "item", "site_id", "structure_local_id"] m
  artifactId <- parseId ArtifactId m
  name <- getNodeText "name" m
  item <- getNode "item" m >>= parseItem
  siteId <- SiteId <$$> getNodeIntMaybe "site_id" m
  structureLocalId <- LocalStructureId <$$> getNodeIntMaybe "structure_local_id" m
    -- if it's been lost lol
  mbSubregion <- getNodeIntMaybe "subregion_id" m
  case mbSubregion of
    Just subregion -> do
      pass
    Nothing -> pass
  {- <abs_tile_x>164736</abs_tile_x>
		<abs_tile_y>124032</abs_tile_y>
		<abs_tile_z>-1000000</abs_tile_z>
		<subregion_id>1435</subregion_id>-}
  pure $ Artifact { artifactId, name, item, siteId, structureLocalId }

parseItem :: (Error Text :> es) => XML.Node -> Eff es Item
parseItem n =  do
  let m = childrenToAssocList n
  expectOnly ["name_string", "page_number", "page_written_content_id", "writing_written_content_id"] m
  name <- getNodeText "name_string" m
  pageNumber <- getNodeIntMaybe "page_number" m
  writtenContentId <- WrittenContentId <$$> coalesceNodeIntMaybe ["page_written_content_id", "writing_written_content_id"] m

  let writtenContent = ItemWrittenContent <$> writtenContentId <*> Just pageNumber
  pure $ Item { name, writtenContent }