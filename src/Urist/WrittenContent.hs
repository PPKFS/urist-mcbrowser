module Urist.WrittenContent where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id
import qualified Data.Text as T

data WrittenContent = WrittenContent
  { author ::

  }

parseWrittenContent :: (IOE :> es, Error Text :> es, State NodeMap :> es) => Eff es WrittenContent
parseWrittenContent = do
  author <- takeNodeAsText "author"
  author_hfid <- takeNodeAsText "author_hfid"
  author_roll <- takeNodeAsText "author_roll"
  form <- takeNodeAsReadable "form"
  writtenContentId <- takeNodeAsInt "id"
  title <- takeNodesAsText "title"
  writtenContentType <- takeNodeAsText "type"
  pure WrittenContent