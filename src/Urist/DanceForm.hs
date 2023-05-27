module Urist.DanceForm where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data DanceForm = DanceForm

parseDanceForm :: (Error Text :> es, State NodeMap :> es) => Eff es DanceForm
parseDanceForm = pure DanceForm