module Urist.Artifact where

import Solitude
import Effectful.Error.Static
import qualified Xeno.DOM as XML
import Urist.ParseHelpers
import Urist.Id

data RegionType =
  Hills | Forest | Grassland | Desert | Lake | Mountains | Wetland | Glacier | Tundra | Ocean
  deriving stock (Show, Read, Ord, Eq, Bounded, Enum, Generic)


data Item = Item
  { name :: Text
  , writtenContent :: Maybe ItemWrittenContent
  } deriving stock (Show)

data ItemWrittenContent = ItemWrittenContent
  { writtenContentId :: WrittenContentId
  , pageCount :: Maybe Int
  } deriving stock (Show)

data SubregionLocation = SubregionLocation
  { subregionId :: RegionId
  , absX :: Int
  , absY :: Int
  , absZ :: Int
  } deriving stock (Show)

data Artifact = Artifact

parseArtifact :: (Error Text :> es, State NodeMap :> es) => Eff es Artifact
parseArtifact = pure Artifact

{- }
data Artifact = Artifact
  { artifactId :: ArtifactId
  , name :: Text
  , item :: Item
  , siteId :: Maybe SiteId
  , structureLocalId :: Maybe LocalStructureId
  , region :: Maybe SubregionLocation
  , holder :: Maybe HistoricalFigureId
  } deriving stock (Show)

firstJustsM :: (Monad m, Foldable f) => f (m (Maybe a)) -> m (Maybe a)
firstJustsM = foldlM go Nothing where
  go :: Monad m => Maybe a -> m (Maybe a) -> m (Maybe a)
  go Nothing         action  = action
  go result@(Just _) _action = return result

parseArtifact :: (Error Text :> es) => Map ByteString XML.Node -> Eff es Artifact
parseArtifact m = do
  expectOnly
    [ "id"
    , "name"
    , "item"
    , "site_id"
    , "structure_local_id"
    , "subregion_id"
    , "abs_tile_x"
    , "abs_tile_y"
    , "abs_tile_z"
    , "holder_hfid"
    ] m
  artifactId <- parseId ArtifactId m
  -- if something doesn't have a name, then we fall back to the name_string of the inner item
  mbName <- firstJustsM
    [ getNodeTextMaybe "name" m
    , getNode "item" m >>= getNodeTextMaybe "name_string" . nodeChildrenToMap
    ]
  name <- maybe
    (throwError $ "Could not find either a name or name_string for artifact with id " <> show artifactId)
    pure mbName
  item <- getNode "item" m >>= parseItem
  siteId <- SiteId <$$> getNodeIntMaybe "site_id" m
  structureLocalId <- LocalStructureId <$$> getNodeIntMaybe "structure_local_id" m
    -- if it's been lost lol
  mbSubregion <- getNodeIntMaybe "subregion_id" m
  region <- case mbSubregion of
    Just subregion -> do
      absX <- getNodeInt "abs_tile_x" m
      absY <- getNodeInt "abs_tile_y" m
      absZ <- getNodeInt "abs_tile_z" m
      let subregionId = RegionId subregion
      pure $ Just $ SubregionLocation { subregionId, absX, absY, absZ }
    Nothing -> pure Nothing
  holder <- HistoricalFigureId <$$> getNodeIntMaybe "holder_hfid" m
  pure $ Artifact { artifactId, name, item, siteId, structureLocalId, region, holder }

parseItem :: (Error Text :> es) => XML.Node -> Eff es Item
parseItem n =  do
  let m = nodeChildrenToMap n
  expectOnly ["name_string", "page_number", "page_written_content_id", "writing_written_content_id"] m
  name <- getNodeText "name_string" m
  pageNumber <- getNodeIntMaybe "page_number" m
  writtenContentId <- WrittenContentId <$$> coalesceNodeIntMaybe ["page_written_content_id", "writing_written_content_id"] m
  let writtenContent = ItemWrittenContent <$> writtenContentId <*> Just pageNumber
  pure $ Item { name, writtenContent }
-}