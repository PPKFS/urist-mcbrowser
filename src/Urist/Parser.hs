module Urist.Parser where

import Solitude
import qualified Xeno.DOM as XML
import Breadcrumbs
import Urist.Region
import Urist.UndergroundRegion
import Urist.ParseHelpers
import Urist.Site
import Urist.Artifact (parseArtifact)
import qualified Data.Map as M
import Data.Attoparsec.Text
import Effectful.Error.Static

data DfWorld

parse :: (Breadcrumbs :> es, (State (Set Text) :> es, Error Text :> es), IOE :> es) => FilePath -> FilePath -> Eff es (Either (CallStack, Text) ())
parse fp fp2 = withSpan' "XML parsing" "df_world" $ do
  vanillaFile <- liftIO $ readFileBS fp
  addAnnotation "Read vanilla legends.xml"
  hackFile <- liftIO $ readFileBS fp2
  addAnnotation "Read legends_plus.xml"
  let vanillaDom = XML.parse vanillaFile
  addAnnotation "legends.xml DOM parsed"
  let plusDom = XML.parse hackFile

  case (vanillaDom, plusDom) of
    (Right v, Right p) -> do
      case buildIntermediateWorldRep v p of
        Left err -> error err
        Right rep -> runError . parseWorld $ rep
      --let removeToplevel x = fromMaybe (error "the dom did not have a top-level node?") $ viaNonEmpty head $ XML.children x)
    x -> error $ show x

parseWorld :: Error Text :> es => Map ByteString (Map Int [XML.Node]) -> Eff es ()
parseWorld m = do
  altName <- asText =<< getGlobalNode "altname" m
  name <- asText =<< getGlobalNode "name" m
  seq m pass

asText :: XML.Node -> Eff es Text
asText = error ""

getGlobalNode :: Error Text :> es => ByteString -> Map ByteString (Map Int [XML.Node]) -> Eff es XML.Node
getGlobalNode n m = case M.lookup n m >>= M.lookup 0 >>= viaNonEmpty head of
  Nothing -> throwError $ "Could not find global node " <> show n
  Just r -> pure r

buildIntermediateWorldRep :: XML.Node -> XML.Node -> Either Text (Map ByteString (Map Int [XML.Node]))
buildIntermediateWorldRep vanilla plus =
  let -- given some node, we want to make it into a id-indexed map
      nodeToIdMap :: XML.Node -> Either Text [(ByteString, Map Int [XML.Node])]
      nodeToIdMap n = do
        traverse (\childNode ->
          if -- these are only in the plus xml so we can cheat a bit (also they don't have ids)
            XML.name childNode `elem` [
              "name"
              , "altname"
              , "creature_raw"
              , "historical_eras"
              , "rivers"
              , "historical_event_relationships"
              , "historical_event_relationship_supplements"
              ]
          then pure (XML.name childNode, M.singleton 0 (XML.children childNode))
          else do
            byIds <- mapM getIdFromNode $ XML.children childNode
            pure (XML.name childNode, M.fromList byIds) ) (XML.children n)
  in do
    -- the children of vmap/pmap are the artifacts, dance_forms, etc.
    v <- M.fromList <$> nodeToIdMap vanilla
    p <- M.fromList <$> nodeToIdMap plus
    pure $ M.unionWith (M.unionWith (<>)) v p

eitherNodeText :: XML.Node -> Either Text Text
eitherNodeText r = case XML.contents r of
  [XML.Text t] -> pure (decodeUtf8 t)
  v -> Left $ "Expected a text node but instead got " <> show v

eitherNodeInt :: XML.Node -> Either Text Int
eitherNodeInt n = do
  t <- eitherNodeText n
  first toText $ parseOnly (signed decimal) t

getIdFromNode :: XML.Node -> Either Text (Int, [XML.Node])
getIdFromNode n = do
  let c' = XML.children n
  idNode <- maybe (Left $ "could not find an id node for " <> show n) Right $ find (\c -> XML.name c == "id") c'
  idNum <- eitherNodeInt idNode
  pure (idNum, c')

parseDom :: (State (Set Text) :> es) => XML.Node -> Eff es () -- [XML.Node]
parseDom root = withExpectedNode' "df_world" root $ do
  let pairs = nodeChildrenToMap root
  regions <- fromListOfEntries "regions" parseRegion pairs
  undergroundRegions <- fromListOfEntries "underground_regions" parseUndergroundRegion pairs
  sites <- fromListOfEntries "sites" parseSite pairs
  --world_constructions <- fromListOfEntries "world_constructions" parseWorldConstruction pairs
  artifacts <- fromListOfEntries "artifacts" parseArtifact pairs
  --liftIO $ forM_ regions print
  pass

  -- insert verification that indeed we are on the top node..
  --map (\n -> parseWorldItem (decodeUtf8 $ XML.name n) n) (XML.children root)
{-}
data WorldItem
parseWorldItem :: Text -> XML.Node -> WorldItem
parseWorldItem = \case
 "sites" -> parseSites
 "world_constructions" -> parseWorldConstructions
 "artifacts" -> parseArtifacts
 "historical_figures" -> parseHistoricalFigures
 "entity_populations" -> parseEntityPopulations
 "entities" -> parseEntities
 "historical_events" -> parseHistoricalEvents
 "historical_event_collections" -> parseHistoricalEventCollections
 "historical_eras" -> parseHistoricalErasnts" -> parseWrittenContents
 "poetic_forms" -> parsePoeticForms
 "musical_forms" -> parseMusicalForms
 "dance_forms" -> parseDanceForms
 x -> failOnUnknownNodeType x
 "written_conte
-}


failOnUnknownNodeType :: Text -> XML.Node -> XML.Node
failOnUnknownNodeType x = const (error $ "could not parse node type " <> x)
