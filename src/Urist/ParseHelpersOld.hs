module Urist.ParseHelpers where

import Effectful.Error.Static
import Solitude
import qualified Xeno.DOM as XML
import qualified Data.Map as M
import Data.Attoparsec.Text
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Bitraversable (bimapM)
import Data.Char

noteError :: State (S.Set Text) :> es => Text -> Eff es ()
noteError = modify . S.insert

withExpectedNode :: ByteString -> XML.Node -> Eff es a -> Eff (Error Text : es) a
withExpectedNode nodeName n go = if nodeName == XML.name n
  then raise go else throwError $ decodeUtf8 $ "Expected node named " <> nodeName <> " but instead it was " <> XML.name n

withExpectedNode' :: ByteString -> XML.Node -> Eff es a -> Eff es a
withExpectedNode' nodeName n go = do
  r <- runError (withExpectedNode nodeName n go)
  case r of
    Left e -> error $ show e
    Right r' -> pure r'

-- | Convert an XML.Node to a map of its children indexed by name.
-- !!! do not use if there may be multiple children with the same name!
nodeChildrenToMap :: XML.Node -> Map ByteString XML.Node
nodeChildrenToMap = M.fromList . map (toFst XML.name) . XML.children

getNodeText :: (Error Text :> es) => ByteString -> Map ByteString XML.Node -> Eff es Text
getNodeText key m = do
  case M.lookup key m of
    Nothing -> throwError $ "could not find key " <> show key <> " in " <> show m
    Just r -> either throwError pure $ checkTextContent r

checkTextContent :: XML.Node -> Either Text Text
checkTextContent r = case XML.contents r of
  [XML.Text t] -> pure (decodeUtf8 t)
  v -> Left $ "Expected a text node but instead got " <> show v

getNodeTextMaybe :: (Error Text :> es) => ByteString -> Map ByteString XML.Node -> Eff es (Maybe Text)
getNodeTextMaybe key m = do
  case M.lookup key m of
    Nothing -> pure Nothing
    Just r
      | [XML.Text t] <- XML.contents r -> pure $ Just (decodeUtf8 t)
    Just v -> throwError $ "Expected a text node but instead got " <> show v

getNodeInt :: (Error Text :> es) => ByteString -> Map ByteString XML.Node -> Eff es Int
getNodeInt key m = do
  t <- getNodeText key m
  case parseOnly (signed decimal) t of
    Left err -> throwError $ toText err
    Right i -> pure i

getNodeIntMaybe :: (Error Text :> es) => ByteString -> Map ByteString XML.Node -> Eff es (Maybe Int)
getNodeIntMaybe key m = do
  t <- getNodeTextMaybe key m
  case parseOnly (signed decimal) <$> t of
    Nothing -> pure Nothing
    Just (Left err) -> throwError $ toText err
    Just (Right i) -> pure $ Just i

expectOnly :: Error Text :> es => [ByteString] -> Map ByteString a -> Eff es ()
expectOnly ids m = if S.difference (S.fromList $ M.keys m) (S.fromList ids) /= S.empty then
  throwError $ "Found excess elements: " <>
    show (S.difference (S.fromList $ M.keys m) (S.fromList ids))
  else pass

asTwoOnly :: (HasCallStack, Show a) => [a] -> (a, a)
asTwoOnly [x,y] = (x, y)
asTwoOnly x = error $ "expected a two elem list and got " <> show x

biParseM :: (HasCallStack, Applicative m) => Text -> (Text -> m b) -> (Text -> m c) -> Text -> m (b, c)
biParseM t l r c = bimapM l r $ asTwoOnly $ T.splitOn t c

biParseM' :: (HasCallStack, Applicative m) => Text -> (Text -> m b) -> Text -> m (b, b)
biParseM' = join . biParseM

forEachNamedChild ::
  Error Text :> es
  => ByteString
  -> (XML.Node -> Eff es a)
  -> XML.Node
  -> Eff es [a]
forEachNamedChild name f = mapM (\n -> subsume $ withExpectedNode name n $ f n) . XML.children

parseMany ::
   Error Text :> es
  => ByteString
  -> (Map ByteString XML.Node -> Eff es a)
  -> XML.Node
  -> Eff es [a]
parseMany name f =
  mapM (\n -> subsume $ withExpectedNode name n $ f (M.singleton name n)) . filter ((name ==) . XML.name) . XML.children

ensureNoDuplicateIds ::
  Error Text :> es
  => XML.Node
  -> Map ByteString XML.Node
  -> Eff es ()
ensureNoDuplicateIds n m =
  if S.difference (S.fromList $ M.keys m) (S.fromList (map XML.name $ XML.children n)) /= S.empty then
    throwError $ "Found duplicated elements: " <>
      show (S.difference (S.fromList $ M.keys m)  (S.fromList (map XML.name $ XML.children n)))
  else pass

fromListOfEntries ::
  (State (Set Text) :> es)
  => ByteString
  -> (Map ByteString XML.Node -> Eff (Error Text : es) a)
  -> Map ByteString XML.Node
  -> Eff es [a]
fromListOfEntries name f lst = do
  let relevantNode = fromMaybe (error $ show $ "Could not find node " <> name) $ M.lookup name lst
  let itemName = encodeUtf8 $ T.init (decodeUtf8 name)
  withExpectedNode' name relevantNode $ mapMaybeM (\n -> do
    runError (subsume $ withExpectedNode itemName n (f (nodeChildrenToMap n))) >>= \case
      Left (_cs, err) -> do
        noteError err
        pure Nothing
      Right r' -> pure $ Just r') (XML.children relevantNode)

parseName :: (Error Text :> es) => Map ByteString XML.Node -> Eff es Text
parseName = getNodeText "name"

convertSiteType :: Text -> Text
convertSiteType = mconcat . map (over _head toUpper) . T.words

parseType :: (Read a, Error Text :> es) => Text -> Map ByteString XML.Node -> Eff es a
parseType n m = do
  t <- getNodeText "type" m

getNodeMaybe :: ByteString -> Map ByteString XML.Node -> Eff es (Maybe XML.Node)
getNodeMaybe = (pure .) . M.lookup

getNode :: Error Text :> es => ByteString -> Map ByteString XML.Node -> Eff es XML.Node
getNode k m = case k `M.lookup` m of
  Nothing -> throwError $ show $ "Could not find node " <> k
  Just r -> pure r

coalesceNodeIntMaybe :: Error Text :> es => [ByteString] -> Map ByteString XML.Node -> Eff es (Maybe Int)
coalesceNodeIntMaybe [] _ = pure Nothing
coalesceNodeIntMaybe (x:xs) m = do
  mbX <- getNodeIntMaybe x m
  case mbX of
    Nothing -> coalesceNodeIntMaybe xs m
    Just r -> pure (Just r)