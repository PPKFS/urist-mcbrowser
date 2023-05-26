module Urist.Site where

import Solitude
import Effectful.Error.Static
import qualified Xeno.DOM as XML
import Urist.ParseHelpers
import qualified Data.EnumMap as EM
import Urist.Structure
import Urist.Id
import qualified Data.Map as M
import qualified Data.Traversable as Trav

data SiteType =
  Hillocks
  | ForestRetreat
  | MountainHalls
  | Shrine
  | Lair
  | Camp
  | Castle
  | DarkFortress
  | DarkPits
  | Fort
  | Fortress
  | Hamlet
  | Labyrinth
  | Monastery
  | Tomb
  | Tower
  | Town
  | Vault
  | Cave
  deriving stock (Show, Read, Ord, Eq, Bounded, Enum, Generic)
type Rectangle = (Coord, Coord)

data SitePropertyType = House deriving stock (Show, Read, Eq)
data SiteProperty = SiteProperty
  { sitePropertyId :: SitePropertyId
  , sitePropertyType :: Maybe SitePropertyType
  , ownerId :: Maybe HistoricalFigureId
  , structureId :: Maybe LocalStructureId
  } deriving stock (Show)

data Site = Site
  { siteId :: SiteId
  , siteType :: SiteType
  , name :: Text
  , coords :: Coord
  , rectangle :: Rectangle
  , structures :: EM.EnumMap LocalStructureId Structure
  , siteProperties :: EM.EnumMap SitePropertyId SiteProperty
  } deriving stock (Show)

parseSite :: (Error Text :> es, State (Set Text) :> es, State NodeMap :> es) => Eff es Site
parseSite = do
  n <- gets M.keys
  siteId <- takeId SiteId
  name <- takeNodeAsText "name"
  siteType <- takeNodeAsReadable "type"
  coords <- asCoord =<< takeNodeAsText "coords"
  --rectangle <- asRectangle =<< takeNodeAsText "rectangle"
  --n <- get @NodeMap


  --if siteId == SiteId 102 then error (show n) else pass
  structureNode <- takeNodeMaybe "structures"
  structures <- maybe EM.empty (listToEnumMap localStructureId) <$> (pure structureNode) `forMM` asNamedChildren "structure" takeStructure
     -- maybe (pure EM.empty)
      --  (fmap (EM.fromList . map (toFst localStructureId)) . forEachNamedChild "structure" parseStructure)
  --siteProperties <- getNodeMaybe "site_properties" m >>=
  --  maybe (pure EM.empty)
  --  (fmap (EM.fromList . map (toFst sitePropertyId)) . forEachNamedChild "site_property" parseSiteProperty)
  _ <- takeNodeAsText "civ_id"
  _ <- takeNodeAsText "rectangle"
  _ <- takeNode "site_properties"
  _ <- takeNodeAsText "cur_owner_id"
  pure $ Site { siteId, name, siteType, coords, structures }

asRectangle :: Text -> Eff es Rectangle
asRectangle = const $ error ""
{-}
parseSiteProperty :: (Error Text :> es) => XML.Node -> Eff es SiteProperty
parseSiteProperty n = do
  let m = nodeChildrenToMap n
  ensureNoDuplicateIds n m
  expectOnly ["id", "type", "owner_hfid", "structure_id"] m
  sitePropertyId <- parseId SitePropertyId m
  sitePropertyType <- mapM (readHelper "site property") =<< getNodeTextMaybe "type" m
  ownerId <- HistoricalFigureId <$$> getNodeIntMaybe "owner_hfid" m
  structureId <- LocalStructureId <$$> getNodeIntMaybe "structure_id" m
  pure $ SiteProperty { sitePropertyId, sitePropertyType, ownerId, structureId }
=
parseRectangle :: (Error Text :> es) => Text -> Eff es Rectangle
parseRectangle = biParseM' ":" parseCoords
-}