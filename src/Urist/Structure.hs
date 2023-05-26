module Urist.Structure where

import Solitude
import Effectful.Error.Static
import qualified Xeno.DOM as XML
import Urist.ParseHelpers
import Urist.Id

data StructureType =
  MeadHall
  | Market
  | Guildhall
  | InnTavern
  | Keep
  | Library LibraryContents
  | Temple HistoricalFigureId
  | Tomb
  | Tower
  | UnderworldSpire
  | Dungeon DungeonType
  | CountingHouse
  deriving stock (Show, Read, Ord, Eq, Generic)

data DungeonType = Catacombs | Sewers
  deriving stock (Show, Read, Ord, Eq, Generic)

newtype LibraryContents = LibraryContents { copiedArtifacts :: [ArtifactId] }
  deriving newtype (Read, Show, Ord, Eq)
  deriving stock (Generic)

data Structure = Structure
  { localStructureId :: LocalStructureId
  , structureType :: StructureType
  , name :: Text
  , entityId :: Maybe EntityId
  } deriving stock (Show)

isLibrary :: StructureType -> Bool
isLibrary (Library _) = True
isLibrary _ = False

isDungeon :: StructureType -> Bool
isDungeon (Dungeon _) = True
isDungeon _ = False

takeStructure :: (Error Text :> es, State NodeMap :> es) => Eff es Structure
takeStructure = do
  localStructureId <- LocalStructureId <$> takeNodeAsInt "local_id"
  name <- takeNodeAsText "name"
  structureTypeNode <- takeNodeAsText "type"
  structureType <- case structureTypeNode of
    "library" -> takeLibraryContents
    "dungeon" -> takeDungeonSubtype
    "temple" -> takeWorshipId
    x -> asReadable "structure" x
  entityId <- EntityId <$$> takeNodeMaybeAsInt "entity_id"
  -- mbCopiedArtifactId <- takeNodeMaybeAsInt "copied_artifact_id"
  -- when (isJust mbCopiedArtifactId && not (isLibrary structureType)) $
  --   throwError (show localStructureId <> "has copied artifacts, but is not a library")
  -- mbSubtype <- takeNodeMaybeAsReadable "subtype"
  -- when (isJust mbSubtype && not (isDungeon structureType)) $
  --  throwError (show localStructureId <> "has a subtype, but is not a dungeon")
  pure $ Structure { localStructureId, name, structureType, entityId }

takeWorshipId :: (Error Text :> es, State NodeMap :> es) => Eff es StructureType
takeWorshipId = Temple . HistoricalFigureId <$> takeNodeAsInt "worship_hfid"

takeDungeonSubtype :: (Error Text :> es, State NodeMap :> es) => Eff es StructureType
takeDungeonSubtype = Dungeon <$> takeNodeAsReadable "subtype"

takeLibraryContents :: (Error Text :> es, State NodeMap :> es) => Eff es StructureType
takeLibraryContents = Library . LibraryContents . map ArtifactId <$> takeNodesMaybeAsInt "copied_artifact_id"
