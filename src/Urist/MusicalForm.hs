module Urist.MusicalForm where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data MusicalForm = MusicalForm

parseMusicalForm :: (Error Text :> es, State NodeMap :> es) => Eff es MusicalForm
parseMusicalForm = pure MusicalForm