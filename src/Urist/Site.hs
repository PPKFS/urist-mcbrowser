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
import Data.Bitraversable (bimapM)

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

data SitePropertyType = House deriving stock (Show, Read, Eq)
data SiteProperty = SiteProperty
  { sitePropertyId :: SitePropertyId
  , contents :: Either LocalStructureId SitePropertyType
  , ownerId :: Maybe HistoricalFigureId
  } deriving stock (Show)

data Site = Site
  { siteId :: SiteId
  , siteType :: SiteType
  , name :: Text
  , coords :: Coord
  , rectangle :: Rectangle
  , structures :: EM.EnumMap LocalStructureId Structure
  -- this is property (real estate) and not property (characteristic)
  , siteProperties :: EM.EnumMap SitePropertyId SiteProperty
  , civilisation :: Maybe CivilisationId
  -- I think this is for a government?
  , currentOwner :: Maybe EntityId
  } deriving stock (Show)

parseSite :: (Error Text :> es, State (Set Text) :> es, State NodeMap :> es) => Eff es Site
parseSite = do
  siteId <- takeId SiteId
  name <- takeNodeAsText "name"
  siteType <- takeNodeAsReadable "type"
  coords <- asCoord =<< takeNodeAsText "coords"
  rectangle <- asRectangle =<< takeNodeAsText "rectangle"
  civilisation <- CivilisationId <$$> takeNodeMaybeAsInt "civ_id"
  currentOwner <- EntityId <$$> takeNodeMaybeAsInt "cur_owner_id"
  structuresNode <- consolidateNodes LocalStructureId =<< takeNodesMaybe "structures"
  sitePropertiesNode <- consolidateNodes SitePropertyId =<< takeNodesMaybe "site_properties"
  structures <- EM.mapMaybe id <$> mapM (takeNodeMapAs "structure" takeStructure) structuresNode
  siteProperties <- EM.mapMaybe id <$> mapM (takeNodeMapAs "site_property" takeSiteProperty) sitePropertiesNode
  pure $ Site { siteId, name, siteType, coords, structures, rectangle, civilisation, currentOwner, siteProperties }

takeSiteProperty :: (Error Text :> es, State NodeMap :> es) => Eff es SiteProperty
takeSiteProperty = do
  sitePropertyId <- SitePropertyId <$> takeNodeAsInt "id"
  contents <- bimapM (LocalStructureId <$$> asInt) (asReadable (show sitePropertyId <> "site property") <=< asText) =<< takeEitherNode "structure_id" "type"
  ownerId <- HistoricalFigureId <$$> takeNodeMaybeAsInt "owner_hfid"
  pure $ SiteProperty { sitePropertyId, contents, ownerId }

-- | the idea is that we have 2 multiple identically named nodes in the list (one from each xml)
-- , which we need to combine into just 1
-- nodemap. but this applies just as well for more than 2 if I want to generalise this later
consolidateNodes :: (Error Text :> es, Enum a) => (Int -> a) -> [XML.Node] -> Eff es (EM.EnumMap a NodeMap)
consolidateNodes f xs = do
  let r = foldl' (\x n -> x <> XML.children n) [] xs
      eithers = mapM (\x -> case eitherNodeId "id" x of
        Left v -> first (v<>) $ eitherNodeId "local_id" x
        Right x' -> Right x') r
  byIds <- throwEither eithers
  pure (EM.fromListWith (<>) $ map (bimap f toItemMap) byIds)
