module Urist.EntityPopulation where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id
import qualified Data.Text as T
import Data.Bitraversable (bimapM)

data CivilisedRace =
  AmphibianMan
  | AntMan
  | BatMan
  | CaveFishMan
  | CaveSwallowMan
  | Dwarf
  | Elf
  | Goblin
  | Human
  | Kobold
  | OlmMan
  | ReptileMan
  | RodentMan
  | SerpentMan
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic, Read)

data EntityPopulation = EntityPopulation
  { entityPopulationId :: EntityPopulationId
  , civilisationId :: CivilisationId
  , race :: CivilisedRace
  , count :: Int
  }

fromSnakeCase :: Text -> Text
fromSnakeCase = T.replace "_" " "

parseEntityPopulation :: (Error Text :> es, State NodeMap :> es) => Eff es EntityPopulation
parseEntityPopulation = do
  entityPopulationId <- takeId EntityPopulationId
  civilisationId <- CivilisationId <$> takeNodeAsInt "civ_id"
  raceNode <- asOnlyTwo . T.split (==':') =<< takeNodeAsText "race"
  (race, count) <- bimapM (asReadable "entity population race" . fromSnakeCase) parseInt raceNode
  pure $ EntityPopulation { entityPopulationId, civilisationId, race, count }