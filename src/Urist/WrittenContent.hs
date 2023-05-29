module Urist.WrittenContent where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id
import qualified Data.Text as T

data WrittenContent = WrittenContent
  { author :: Text
  , authorId :: HistoricalFigureId
  -- I have no idea what this is.
  , authorRoll :: Int
  , form :: WrittenContentForm
  }

data WrittenContentForm =
  Atlas
  | Autobiography
  | Biography
  | Choreography
  | Chronicle
  | ComparativeBiography
  | CulturalComparison
  | CulturalHistory
  | Dialog
  | Dictionary
  | Encyclopedia
  | Essay
  | Genealogy
  | Guide
  | Letter
  | Manual
  | MusicalComposition
  | Novel
  | Play
  | Poem
  | ShortStory
  | StarChart
  | TreatiseOnTechnologicalEvolution
  deriving stock (Eq, Enum, Read, Ord, Bounded)

parseWrittenContent :: (IOE :> es, Error Text :> es, State NodeMap :> es) => Eff es WrittenContent
parseWrittenContent = do
  author <- takeNodeAsText "author"
  authorId <- HistoricalFigureId <$> takeNodeAsInt "author_hfid"
  author_roll <- takeNodeAsText "author_roll"
  form <- takeNodeAsReadable "form"
  writtenContentId <- takeNodeAsInt "id"
  title <- takeNodesAsText "title"
  writtenContentType <- takeNodeAsText "type"
  pure $ WrittenContent { author, authorId, form }