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