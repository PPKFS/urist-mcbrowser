module Urist.PoeticForm where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data PoeticForm = PoeticForm

parsePoeticForm :: (Error Text :> es, State NodeMap :> es) => Eff es PoeticForm
parsePoeticForm = pure PoeticForm