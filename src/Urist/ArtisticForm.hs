{-# LANGUAGE AllowAmbiguousTypes #-}
module Urist.ArtisticForm where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data ArtisticFormKind = Dance | Poetic | Musical

class AsArtisticForm (kind :: ArtisticFormKind) where
  type ArtisticFormId kind
  toId :: Proxy kind -> Int -> ArtisticFormId kind

instance AsArtisticForm 'Dance where
  type ArtisticFormId _ = DanceFormId
  toId _ = DanceFormId

instance AsArtisticForm 'Poetic where
  type ArtisticFormId _ = PoeticFormId
  toId _ = PoeticFormId

instance AsArtisticForm 'Musical where
  type ArtisticFormId _ = MusicalFormId
  toId _ = MusicalFormId

data ArtisticForm (kind :: ArtisticFormKind) = ArtisticForm
  { artisticFormId :: ArtisticFormId kind
  , name :: Text
  , description :: Text
  }

parseArtisticForm :: forall k es. (Error Text :> es, State NodeMap :> es, AsArtisticForm k) => Proxy (k :: ArtisticFormKind) -> Eff es (ArtisticForm k)
parseArtisticForm _ = do
  artisticFormId <- toId @k Proxy <$> takeNodeAsInt "id"
  name <- takeNodeAsText "name"
  description <- takeNodeAsText "description"
  pure $ ArtisticForm { artisticFormId, name, description }