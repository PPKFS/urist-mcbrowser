module Urist.ParseHelpers where

import Solitude
import qualified Xeno.DOM as XML
import qualified Data.Map as M
import Effectful.Error.Static
import Data.Attoparsec.Text
import Effectful.Optics ((%%=))
import qualified Data.Text as T
import Data.Char (toUpper)

newtype XMLItem = XMLItem (Either XML.Node [XML.Node])
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
eitherNodeInt = eitherNodeText >=> first toText . parseOnly (signed decimal)

eitherNodeId :: XML.Node -> Either Text (Int, [XML.Node])
eitherNodeId n = do
  let c' = XML.children n
  idNode <- maybe (Left $ "could not find an id node for " <> show n) Right $ find (\c -> XML.name c == "id") c'
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

takeId :: (Error Text :> es, State NodeMap :> es) => (Int -> a) -> Eff es a
takeId = flip fmap (asInt =<< takeNode "id")

asInt :: Error Text :> es => XML.Node -> Eff es Int
asInt = throwEither . eitherNodeInt

toReadableValue :: Text -> Text
toReadableValue = mconcat . map (over _head toUpper) . T.words

asReadable :: (Read a, Error Text :> es) => Text -> Text -> Eff es a
asReadable n t = throwMaybe ("Unknown " <> n <> " type: " <> toText t) . readMaybe . toString . toReadableValue $ t

takeNodeAsReadable :: (Read a, Error Text :> es, State NodeMap :> es) => ByteString -> Eff es a
takeNodeAsReadable = liftA2 (>>=) takeNodeAsText (asReadable . decodeUtf8)

takeNode :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es XML.Node
takeNode = takeXmlItem >=> \case
  XMLItem (Left x) -> pure x
  XMLItem (Right [x]) -> pure x
  XMLItem (Right []) -> throwError "Found no nodes at all. Impossible?"
  XMLItem (Right (x:_)) -> case XML.name x of
    "id" -> pure x
    other -> throwError $ "Found multiple matching nodes when expecting 1: " <> show other

takeXmlItem :: (Error Text :> es, State NodeMap :> es) => ByteString -> Eff es XMLItem
takeXmlItem k = simple %%= M.updateLookupWithKey (\_ _ -> Nothing) k >>= throwMaybe ("Could not find node by name: " <> show k)
