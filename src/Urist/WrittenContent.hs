module Urist.WrittenContent where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Xeno.DOM as XML

data WrittenContent = WrittenContent
  { author :: Text
  , authorId :: HistoricalFigureId
  -- I have no idea what this is.
  , authorRoll :: Int
  , form :: WrittenContentForm
  , styles :: S.Set WrittenContentStyle
  , writtenContentId :: WrittenContentId
  , title :: Text
  , pageEnd :: Maybe Int
  , reference :: Maybe WrittenContentReference
  }

data WrittenContentForm =
  Atlas
  | Autobiography
  | Biography
  | Choreography DanceFormId
  | Chronicle (Maybe PoeticFormId)
  | ComparativeBiography
  | CulturalComparison
  | CulturalHistory
  | Dialog
  | Dictionary
  | Encyclopedia
  | Essay (Maybe PoeticFormId)
  | Genealogy
  | Guide (Maybe PoeticFormId)
  | Letter (Maybe PoeticFormId)
  | Manual (Maybe PoeticFormId)
  | MusicalComposition MusicalFormId
  | Novel (Maybe PoeticFormId)
  | Play (Maybe PoeticFormId)
  | Poem PoeticFormId
  | ShortStory (Maybe PoeticFormId)
  | StarChart
  | TreatiseOnTechnologicalEvolution
  deriving stock (Eq, Read, Ord, Show)

data WrittenContentStyle = WrittenContentStyle
  { style :: Style
  , amount :: Int
  } deriving stock (Eq, Show, Ord, Read, Generic)

data Style =
  Cheerful
  | Compassionate
  | Concise
  | Disjointed
  | Florid
  | Humorous
  | Meandering
  | Mechanical
  | Melancholy
  | Puerile
  | Rant
  | Sardonic
  | SelfIndulgent
  | Serious
  | Tender
  | Vicious
  | Witty
  | Forceful
  deriving stock (Eq, Show, Ord, Read, Generic)

data WrittenContentReference =
  HistoricalEvent HistoricalEventId
  | HistoricalFigure HistoricalFigureId
  | DanceForm DanceFormId
  | Interaction
  | SpecificKnowledge
  | Language
  | MusicalForm MusicalFormId
  | PoeticForm PoeticFormId
  | Site SiteId
  | Subregion RegionId
  | Value
  | WrittenContentRef WrittenContentId

  deriving stock (Show, Read)

parseWrittenContent :: (Error Text :> es, State (Set Text) :> es, State NodeMap :> es) => Eff es WrittenContent
parseWrittenContent = do
  author <- takeNodeAsText "author"
  authorId <- HistoricalFigureId <$> takeNodeAsInt "author_hfid"
  authorRoll <- takeNodeAsInt "author_roll"
  formNodeText <- takeNodeAsText "form"
  form <- case formNodeText of
    "choreography" -> Choreography . DanceFormId <$> withFormId
    "musical composition" -> MusicalComposition . MusicalFormId <$> withFormId
    "chronicle" -> Chronicle <$> withPoeticFormMaybe
    "essay" -> Essay <$> withPoeticFormMaybe
    "poem" -> Poem . PoeticFormId <$> withFormId
    "guide" -> Guide <$> withPoeticFormMaybe
    "letter" -> Letter <$> withPoeticFormMaybe
    "manual" -> Manual <$> withPoeticFormMaybe
    "play" -> Play <$> withPoeticFormMaybe
    "novel" -> Novel <$> withPoeticFormMaybe
    "short story" -> ShortStory <$> withPoeticFormMaybe
    x -> asReadable "written content form" x
  writtenContentId <- WrittenContentId <$> takeNodeAsInt "id"
  title <- consolidateTitles =<< takeNodesAsText "title"
  reference <- mapM asReference =<< takeNodeMaybe "reference"
  -- this should be the same as the form, plus/minus some capitalisation change
  _writtenContentType <- takeNodeAsText "type"
  pageStart <- takeNodeMaybeAsInt "page_start"
  verifyThat
    [ (pageStart == Just 1 || isNothing pageStart, "page_start should always be 1")
    ]
  -- this indeed appears to be independent of form
  pageEnd <- takeNodeMaybeAsInt "page_end"
  formIdLeftOver <- takeNodeMaybeAsText "form_id"
  whenJust formIdLeftOver (const $ throwError $ "form_id not handled for type: " <> show form <> show title)
  styles <- asStyles =<< takeNodesMaybeAsText "style"
  pure $ WrittenContent { author, authorId, authorRoll, form, styles, writtenContentId, title, pageEnd, reference }

asReference :: (Error Text :> es, State (Set Text) :> es) => XML.Node -> Eff es WrittenContentReference
asReference n = do
  r <- takeNodeMapAs "references" f . toItemMap . XML.children $ n
  throwMaybe "could not parse the reference" r
  where
    f = do
      referenceTypeRaw <- takeNodeAsText "type"
      case referenceTypeRaw of
        "DANCE_FORM" -> DanceForm <$> takeId DanceFormId
        "HISTORICAL_EVENT" -> HistoricalEvent <$> takeId HistoricalEventId
        "HISTORICAL_FIGURE" -> HistoricalFigure <$> takeId HistoricalFigureId
        "INTERACTION" -> pure Interaction
        "KNOWLEDGE_SCHOLAR_FLAG" -> pure SpecificKnowledge
        "LANGUAGE" -> pure Language
        "MUSICAL_FORM" -> MusicalForm <$> takeId MusicalFormId
        "POETIC_FORM" -> PoeticForm <$> takeId PoeticFormId
        "SITE" -> Site <$> takeId SiteId
        "SUBREGION" -> Subregion <$> takeId RegionId
        "VALUE_LEVEL" -> pure Value
        "WRITTEN_CONTENT" -> WrittenContentRef <$> takeId WrittenContentId
        x -> throwError $ "Unknown written content reference :" <> x

withFormId :: (Error Text :> es, State NodeMap :> es) => Eff es Int
withFormId = takeNodeAsInt "form_id"

withPoeticFormMaybe :: (Error Text :> es, State NodeMap :> es) => Eff es (Maybe PoeticFormId)
withPoeticFormMaybe = PoeticFormId <$$> takeNodeMaybeAsInt "form_id"

asStyles :: Error Text :> es => [Text] -> Eff es (Set WrittenContentStyle)
asStyles = (fmap S.fromList .) . mapMaybeM $ \s -> do
  let sSplit = T.split (==':') s
  case sSplit of
    -- a legends_plus style, ignore
    [_] -> pure Nothing
    [styleString, numString] -> do
      num <- parseInt numString
      style <- asReadable "written content style" styleString
      pure $ Just $ WrittenContentStyle style num
    x -> throwError $ "when parsing a style, encountered " <> show x

-- duplicated, but the second is UTF-8 so it avoids possible formatting annoyance
consolidateTitles :: Error Text :> es => NonEmpty Text -> Eff es Text
consolidateTitles (_ :| [y]) = pure y
consolidateTitles (x :| []) = pure x
consolidateTitles v = throwError $ "expected at most two items but found " <> show v
