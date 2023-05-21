module Urist.HistoricalFigure where

import Solitude

newtype HistoricalFigureId = HistoricalFigureId Int
  deriving newtype (Show, Ord, Eq, Num, Bounded, Enum)
  deriving stock (Generic)