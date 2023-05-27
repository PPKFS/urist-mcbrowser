module Urist.ParseHelpers where

import Solitude
import qualified Xeno.DOM as XML
import qualified Data.Map as M
import Effectful.Error.Static
import Data.Attoparsec.Text
import Effectful.Optics ((%%=))
import qualified Data.Text as T
import Data.Char (toUpper)
import qualified Data.Set as S
import Urist.Id
import qualified Data.EnumMap as EM
import qualified Data.Traversable as Trav

newtype XMLItem = XMLItem (Either XML.Node [XML.Node]) deriving newtype (Show)
type NodeMap = Map ByteString XMLItem

throwEither :: Error a :> es => Either a b -> Eff es b
throwEither = either throwError pure

throwMaybe :: Error b :> es => b -> Maybe a -> Eff es a
throwMaybe = flip maybe pure . throwError

eitherNodeText :: XML.Node -> Either Text Text
eitherNodeText r = case XML.contents r of
  [XML.Text t] -> pure (decodeUtf8 t)
  v -> Left $ "Expected a text node but instead got " <> show v

eitherNodeInt :: XML.Node -> Either Text Int
eitherNodeInt = eitherNodeText >=> eitherParseInt

eitherNodeId :: ByteString -> XML.Node -> Either Text (Int, [XML.Node])
eitherNodeId idName n = do
  let c' = XML.children n
  idNode <- maybe (Left $ "could not find an id node for " <> show n) Right $ find (\c -> XML.name c == idName) c'
  idNum <- eitherNodeInt idNode
  pure (idNum, c')

-- | Convert a list of nodes to a name-indexed map, and consolidate possible multiple-nodes.
toItemMap :: [XML.Node] -> Map ByteString XMLItem
toItemMap = M.map (XMLItem . \case
  [x] -> Left x
  xs -> Right xs) . M.fromListWith (<>) . map (XML.name &&& one)

asText :: Error Text :> es => XML.Node -> Eff es Text
asText = either throwError pure . eitherNodeText

takeNodeAsText :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es Text
takeNodeAsText = takeNode >=> asText

takeNodeMaybeAsText :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es (Maybe Text)
takeNodeMaybeAsText = takeNodeMaybe >==> asText

takeId :: (Error Text :> es, State NodeMap :> es) => (Int -> a) -> Eff es a
takeId = flip fmap (asInt =<< takeNode "id")

asInt :: Error Text :> es => XML.Node -> Eff es Int
asInt = throwEither . eitherNodeInt

takeNodeAsInt :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es Int
takeNodeAsInt = takeNode >=> asInt

-- | loooooooong feesh
(>==>) :: (Traversable f, Monad m) => (a -> m (f b)) -> (b -> m c) -> a -> m (f c)
(>==>) = (. mapM) . flip . ((>>=) .)

takeNodeMaybeAsInt :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es (Maybe Int)
takeNodeMaybeAsInt = takeNodeMaybe >==> asInt

toReadableValue :: Text -> Text
toReadableValue = mconcat . map (over _head toUpper) . T.words

asReadable :: (Read a, Error Text :> es) => Text -> Text -> Eff es a
asReadable n t = throwMaybe ("Unknown " <> n <> " type: " <> toText t) . readMaybe . toString . toReadableValue $ t

takeNodeAsReadable :: (Read a, Error Text :> es, State NodeMap :> es) => ByteString -> Eff es a
takeNodeAsReadable = liftA2 (>>=) takeNodeAsText (asReadable . decodeUtf8)

takeNodeMaybeAsReadable :: (Read a, Error Text :> es, State NodeMap :> es) => ByteString -> Eff es (Maybe a)
takeNodeMaybeAsReadable = (takeNodeMaybeAsText >==>) =<< asReadable . decodeUtf8

takeNodeMaybe :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es (Maybe XML.Node)
takeNodeMaybe = fmap rightToMaybe . tryError . takeNode

takeNode :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es XML.Node
takeNode = takeXmlItem >=> \case
  XMLItem (Left x) -> pure x
  XMLItem (Right [x]) -> pure x
  XMLItem (Right []) -> throwError "Found no nodes at all. Impossible?"
  XMLItem (Right l@(x:_)) -> case XML.name x of
    "id" -> pure x
    other -> error $ "Found multiple matching nodes when expecting 1: " <> show other <> show l <> show (length l)

takeNodes :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es (NonEmpty XML.Node)
takeNodes n = takeNodesMaybe n >>= \case
  [] -> throwError $ "Expected 1 or more nodes but found none" <> show n
  (x:xs) -> pure $ x :| xs

takeNodesMaybe :: (State NodeMap :> es) => ByteString -> Eff es [XML.Node]
takeNodesMaybe = takeXmlItemMaybe >=> \case
  Nothing -> pure []
  Just (XMLItem (Left x)) -> pure [x]
  Just (XMLItem (Right xs)) -> pure xs

takeNodesMaybeAsInt :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es [Int]
takeNodesMaybeAsInt = takeNodesMaybe >=> mapM asInt

takeXmlItemMaybe :: (State NodeMap :> es) => ByteString -> Eff es (Maybe XMLItem)
takeXmlItemMaybe k = simple %%= M.updateLookupWithKey (\_ _ -> Nothing) k

takeXmlItem :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es XMLItem
takeXmlItem k = simple %%= M.updateLookupWithKey (\_ _ -> Nothing) k >>= throwMaybe ("Could not find node by name: " <> show k)

eitherParseInt :: Text -> Either Text Int
eitherParseInt = first toText . parseOnly (signed decimal)

parseInt :: (Error Text :> es) => Text -> Eff es Int
parseInt = throwEither . eitherParseInt

asCoordList :: (Error Text :> es) => Text -> Eff es (S.Set Coord)
asCoordList = fmap S.fromList . mapM asCoord . T.split (=='|') . T.init

asCoord :: (Error Text :> es) => Text -> Eff es Coord
asCoord input = case T.split (==',') input of
  [x, y] -> (,) <$> parseInt x <*> parseInt y
  x -> throwError $ "expected a coordinate but got " <> show x

asRectangle :: (Error Text :> es) => Text -> Eff es Rectangle
asRectangle = asOnlyTwo <=< mapM asCoord . T.split (==':') . T.init

asOnlyTwo :: (Error Text :> es, Show a) =>  [a] -> Eff es (a, a)
asOnlyTwo [x, y] = pure (x, y)
asOnlyTwo x = throwError $ "Expected only two items but found " <> show x

takeNodeMapAs :: (State (Set Text) :> es) => ByteString -> Eff (Error Text : State NodeMap : es) a -> NodeMap -> Eff es (Maybe a)
takeNodeMapAs n f = flip evalStateLocal $ do
  res <- runError $ do
    p <- f
    s <- get
    when (M.size s > 0) $ throwError $ "When parsing " <> show n <> " the following elements weren't used: " <> show (M.keys s)
    pure p
  case res of
    Left (_callStack, e) -> modify (S.insert e) >> pure Nothing
    Right r' -> pure $ Just r'

listToEnumMap :: Enum b => (a -> b) -> [a] -> EM.EnumMap b a
listToEnumMap = (EM.fromList .) . map . toFst

asNamedChildren :: (State (Set Text) :> es, Error Text :> es) => ByteString -> Eff (State NodeMap : es) a -> XML.Node -> Eff es [a]
asNamedChildren bs f n =
  fmap catMaybes $ forM (XML.children n) $ \c ->
    if XML.name c == bs then takeNodeMapAs bs (raise f) (toItemMap (XML.children c)) else throwError $
      "Expected a node named " <> show bs <> " and it was named " <> show (XML.name c)

mapMM :: (Traversable t, Monad m) => (a -> m b) -> m (t a) -> m (t b)
mapMM f mxs = Trav.mapM f =<< mxs

forMM :: (Traversable t, Monad m) => m (t a) -> (a -> m b) -> m (t b)
forMM = flip mapMM

verifyThat :: Error Text :> es => [(Bool, Text)] -> Eff es ()
verifyThat = mapM_ (liftA2 unless fst (throwError . snd))

takeEitherNode ::  (Error Text :> es, State NodeMap :> es) => ByteString -> ByteString -> Eff es (Either XML.Node XML.Node)
takeEitherNode = (. flip (maybe . (Right <$>) . takeNode) (pure . Left)) . (>>=) . takeNodeMaybe