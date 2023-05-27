module Urist.Structure where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data StructureType =
  MeadHall
  | Market
  | Guildhall
  | InnTavern
  | Keep
  | Library LibraryContents
  | Temple DevotedTo
  | Tomb
  | Tower
  | UnderworldSpire
  | Dungeon DungeonType
  | CountingHouse
  deriving stock (Show, Read, Ord, Eq, Generic)

data DevotedTo = ReligionDevotion EntityId | DeityDevotion HistoricalFigureId
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
  localStructureId <- LocalStructureId <$> do
    -- of course, vanilla and dfhack give these different names..
    lid <- takeNodeAsInt "local_id"
    rid <- takeNodeAsInt "id"
    if lid /= rid then throwError ("found mismatching ids; " <> show (lid, rid)) else pure lid
  name <- takeNodeAsText "name"
  structureTypeNode <- takeNodeAsText "type"
  structureType <- case structureTypeNode of
    "library" -> takeLibraryContents
    "dungeon" -> takeDungeonSubtype
    "temple" -> takeTempleDevotion
    x -> asReadable "structure" x
  entityId <- EntityId <$$> takeNodeMaybeAsInt "entity_id"
  -- mbCopiedArtifactId <- takeNodeMaybeAsInt "copied_artifact_id"
  -- when (isJust mbCopiedArtifactId && not (isLibrary structureType)) $
  --   throwError (show localStructureId <> "has copied artifacts, but is not a library")
  -- mbSubtype <- takeNodeMaybeAsReadable "subtype"
  -- when (isJust mbSubtype && not (isDungeon structureType)) $
  --  throwError (show localStructureId <> "has a subtype, but is not a dungeon")
  pure $ Structure { localStructureId, name, structureType, entityId }

-- | If a temple was created independently of a religion (but to a deity), it has a hfid. this is rare
-- if it was created by a religion, it has an entity_id.
takeTempleDevotion :: (Error Text :> es, State NodeMap :> es) => Eff es StructureType
takeTempleDevotion = do
  -- vanilla is either of these two
  hfid <- HistoricalFigureId <$$> takeNodeMaybeAsInt "worship_hfid"
  eid <- EntityId <$$> takeNodeMaybeAsInt "entity_id"
  -- and it seems in the dfhack file it's very explicitly spelled out, but we've already worked it out
  -- and these fields are identical to the above, or *should* be..
  eid' <- EntityId <$$> takeNodeMaybeAsInt "religion"
  hfid' <- HistoricalFigureId <$$> takeNodeMaybeAsInt "deity"
  dt <- takeNodeAsInt "deity_type"
  verifyThat
    [ (eid' == eid, "religion (in legends_plus) should be the same as entity_id")
    , (hfid' == hfid, "deity (in legends plus) should be the same as worship_hfid")
    , (if dt == 0 then isJust hfid' else isJust eid', "")
    ]

  case (hfid, eid) of
    (Just hf, Nothing) -> pure $ Temple . DeityDevotion $ hf
    (Nothing, Just e) -> pure $ Temple . ReligionDevotion $ e
    x -> throwError $ "Expected a temple to have either an entity id xor a deity id but found " <> show x

takeDungeonSubtype :: (Error Text :> es, State NodeMap :> es) => Eff es StructureType
takeDungeonSubtype = Dungeon <$> takeNodeAsReadable "subtype"

takeLibraryContents :: (Error Text :> es, State NodeMap :> es) => Eff es StructureType
takeLibraryContents = Library . LibraryContents . map ArtifactId <$> takeNodesMaybeAsInt "copied_artifact_id"
