module Urist.WrittenContent where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data WrittenContent = WrittenContent

parseWrittenContent :: (Error Text :> es, State NodeMap :> es) => Eff es WrittenContent
parseWrittenContent = pure WrittenContent