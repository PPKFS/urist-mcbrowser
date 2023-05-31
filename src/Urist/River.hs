module Urist.River where

import Solitude
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id
import qualified Data.Text as T

data River = River
  { path :: [RiverPathPiece]
  , name :: Text
  , endPosition :: Coord
  }

data RiverPathPiece = RiverPathPiece
  { coord :: Coord
  , flow :: Int
  , exitTile :: Int
  , elevation :: Int
  }

parseRiver :: (Error Text :> es, State NodeMap :> es) => Eff es River
parseRiver = do
  name <- takeNodeAsText "name"
  endPosition <- asCoord =<< takeNodeAsText "end_pos"
  path <- as5TupleList =<< takeNodeAsText "path"
  pure $ River { name, endPosition, path }

as5TupleList :: (Error Text :> es) => Text -> Eff es [RiverPathPiece]
as5TupleList = mapM (fmap (\(x1, x2, f, e, el) -> RiverPathPiece (x1, x2) f e el) . as5Tuple) . T.split (=='|') . T.init

as5Tuple :: (Error Text :> es) => Text -> Eff es (Int, Int, Int, Int, Int)
as5Tuple t = do
  r <- mapM parseInt $ T.split (==',') t
  case r of
    [a,b,c,d,e] -> pure (a,b,c,d,e)
    x -> throwError $ "expected a 5 tuple but found " <> show x
