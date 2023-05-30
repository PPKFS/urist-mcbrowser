module Urist.Id where

import Solitude

newtype ArtifactId = ArtifactId Int
  deriving newtype (Show, Read, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype SiteId = SiteId Int
  deriving newtype (Show, Read, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype SitePropertyId = SitePropertyId Int
  deriving newtype (Show, Read, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype RegionId = RegionId Int
  deriving newtype (Show, Read, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype LocalStructureId = LocalStructureId Int
  deriving newtype (Show, Read, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype EntityId = EntityId Int
  deriving newtype (Show, Read, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype WrittenContentId = WrittenContentId Int
  deriving newtype (Show, Read, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype HistoricalFigureId = HistoricalFigureId Int
  deriving newtype (Show, Read, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype WorldConstructionId = WorldConstructionId Int
  deriving newtype (Show, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype ForceId = ForceId Int
  deriving newtype (Show, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype CivilisationId = CivilisationId Int
  deriving newtype (Show, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)

newtype LandmassId = LandmassId Int
  deriving newtype (Show, Ord, Eq, Num, Read, Bounded, Enum)
  deriving stock (Generic)

newtype PoeticFormId = PoeticFormId Int
  deriving newtype (Show, Ord, Eq, Num, Read, Bounded, Enum)
  deriving stock (Generic)

newtype DanceFormId = DanceFormId Int
  deriving newtype (Show, Ord, Eq, Num, Read, Bounded, Enum)
  deriving stock (Generic)

newtype MusicalFormId = MusicalFormId Int
  deriving newtype (Show, Ord, Eq, Num, Read, Bounded, Enum)
  deriving stock (Generic)

newtype HistoricalEventId = HistoricalEventId Int
  deriving newtype (Show, Ord, Eq, Num, Read, Bounded, Enum)
  deriving stock (Generic)


type Coord = (Int, Int)

type Rectangle = (Coord, Coord)

data Name = Name
  { name :: Maybe Text
  , translatedName :: Text
  } deriving stock (Eq, Show, Read, Ord, Generic)