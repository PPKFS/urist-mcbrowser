module Urist.WorldConstruction where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data WorldConstructionType =
    Bridge
    | Road
    | Tunnel
  deriving stock (Show, Read, Ord, Eq, Bounded, Enum, Generic)

data WorldConstruction = WorldConstruction
  { worldConstructionId :: WorldConstructionId
  , name :: Text
  , worldConstructionType :: WorldConstructionType
  , coords :: Set Coord
  } deriving stock (Show)

parseWorldConstruction :: (Error Text :> es, State NodeMap :> es) => Eff es WorldConstruction
parseWorldConstruction = do
  worldConstructionId <- takeId WorldConstructionId
  name <- takeNodeAsText "name"
  worldConstructionType <- takeNodeAsReadable "type"
  coords <- asCoordList =<< takeNodeAsText "coords"
  pure $ WorldConstruction { worldConstructionId, name, coords, worldConstructionType }
