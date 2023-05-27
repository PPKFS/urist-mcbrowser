module Urist.Structure where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id
import qualified Data.EnumSet as ES

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
  , name :: Name
  , inhabitants :: ES.EnumSet HistoricalFigureId
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
  name1 <- takeNodeAsText "name"
  name2 <- takeNodeAsText "name2"
  let name = Name (Just name2) name1

  structureTypeNode <- takeNodeAsText "type"
  structureType <- case structureTypeNode of
    "library" -> takeLibraryContents
    "dungeon" -> takeDungeonSubtype
    "temple" -> takeTempleDevotion
    x -> asReadable "structure" x
  inhabitants <- ES.fromList . fmap HistoricalFigureId <$> takeNodesMaybeAsInt "inhabitant"
  pure $ Structure { localStructureId, name, structureType, inhabitants }

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
    , (if dt == 0 then isJust hfid' else isJust eid', "deity_type=0 should be with only a deity, 1 with a religion")
    ]
  case (hfid, eid) of
    (Just hf, Nothing) -> pure $ Temple . DeityDevotion $ hf
    (Nothing, Just e) -> pure $ Temple . ReligionDevotion $ e
    x -> throwError $ "Expected a temple to have either an entity id xor a deity id but found " <> show x

takeDungeonSubtype :: (Error Text :> es, State NodeMap :> es) => Eff es StructureType
takeDungeonSubtype = do
  dst <- takeNodeAsReadable "subtype"
  dungeonType <- takeNodeAsInt "dungeon_type"
  verifyThat
    [ (if dungeonType == 1 then dst == Sewers else dst == Catacombs, "dungeon_type=1 is sewers, 2 is catacombs")
    ]
  pure $ Dungeon dst

takeLibraryContents :: (Error Text :> es, State NodeMap :> es) => Eff es StructureType
takeLibraryContents = Library . LibraryContents . map ArtifactId <$> takeNodesMaybeAsInt "copied_artifact_id"