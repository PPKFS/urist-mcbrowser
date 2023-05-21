module Urist.Structure where


import Solitude
import Effectful.Error.Static
import qualified Xeno.DOM as XML
import Urist.ParseHelpers
import Urist.HistoricalFigure
import qualified Data.Map as M
import Urist.Artifact
import Urist.Id

data StructureType =
  MeadHall
  | Market
  | Guildhall
  | InnTavern
  | Keep
  | Library LibraryContents
  | Temple
  | Tomb
  | Tower
  | UnderworldSpire
  | Dungeon
  | CountingHouse
  deriving stock (Show, Read, Ord, Eq, Generic)

newtype LibraryContents = LibraryContents { copiedArtifacts :: [ArtifactId] }
  deriving newtype (Read, Show, Ord, Eq)
  deriving stock (Generic)

data Structure = Structure
  { localStructureId :: LocalStructureId
  , structureType :: StructureType
  , name :: Text
  , entityId :: Maybe EntityId
  , worshipId :: Maybe HistoricalFigureId
  } deriving stock (Show)

isLibrary :: StructureType -> Bool
isLibrary (Library _) = True
isLibrary _ = False

parseStructure :: (Error Text :> es) => XML.Node -> Eff es Structure
parseStructure n = do
  let m = childrenToAssocList n
  expectOnly ["local_id", "type", "name", "entity_id", "worship_hfid", "copied_artifact_id"] m
  localStructureId <- LocalStructureId <$> getNodeInt "local_id" m
  name <- parseName m
  structureTypeNode <- getNodeText "type" m
  structureType <- case structureTypeNode of
    "library" -> parseLibraryContents n
    x -> readHelper "structure" x
  entityId <- EntityId <$$> getNodeIntMaybe "entity_id" m
  worshipId <- HistoricalFigureId <$$> getNodeIntMaybe "worship_hfid" m
  mbCopiedArtifactId <- getNodeMaybe "copied_artifact_id" m
  if isJust mbCopiedArtifactId && not (isLibrary structureType)
    then throwError (show n <> "has copied artifacts, but is not a library") else pass
  pure $ Structure { localStructureId, name, structureType, entityId, worshipId }

parseLibraryContents :: (Error Text :> es) => XML.Node -> Eff es StructureType
parseLibraryContents n = do
  copiedArtifactIds <- map ArtifactId <$> parseMany "copied_artifact_id" (getNodeInt "copied_artifact_id") n
  pure $ Library (LibraryContents copiedArtifactIds)