module Urist.Site where

import Solitude
import Effectful.Error.Static
import qualified Xeno.DOM as XML
import Urist.ParseHelpers
import qualified Data.EnumMap as EM
import Data.Attoparsec.Text
import Urist.Structure
import Urist.HistoricalFigure
import Urist.Id

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

type Coord = (Int, Int)
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

parseSite :: (Error Text :> es) => Map ByteString XML.Node -> Eff es Site
parseSite m = do
  expectOnly ["id", "type", "name", "coords", "rectangle", "structures", "site_properties"] m
  siteId <- parseId SiteId m
  name <- parseName m
  siteType <- parseType "site" m
  coords <- getNodeText "coords" m >>= parseCoords
  rectangle <- getNodeText "rectangle" m >>= parseRectangle
  structures <- getNodeMaybe "structures" m >>=
    maybe (pure EM.empty)
      (fmap (EM.fromList . map (toFst localStructureId)) . forEachNamedChild "structure" parseStructure)
  siteProperties <- getNodeMaybe "site_properties" m >>=
    maybe (pure EM.empty)
    (fmap (EM.fromList . map (toFst sitePropertyId)) . forEachNamedChild "site_property" parseSiteProperty)
  pure $ Site { siteId, name, siteType, coords, rectangle, structures, siteProperties }

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

parseCoords :: (Error Text :> es) => Text -> Eff es Coord
parseCoords = biParseM' "," tryParseInt

parseRectangle :: (Error Text :> es) => Text -> Eff es Rectangle
parseRectangle = biParseM' ":" parseCoords

tryParseInt :: (Error Text :> es) => Text -> Eff es Int
tryParseInt t = case parseOnly (signed decimal) t of
    Left err -> throwError $ toText err
    Right i -> pure i
