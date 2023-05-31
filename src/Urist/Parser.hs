module Urist.Parser where

import Solitude
import qualified Xeno.DOM as XML
import Breadcrumbs
import Urist.Region
--import Urist.UndergroundRegion
--import Urist.Site
--import Urist.Artifact (parseArtifact)
import qualified Data.Map as M
import Urist.ParseHelpers
import Effectful.Error.Static
import Urist.UndergroundRegion
import qualified Data.Set as S
import Urist.Site
import Effectful.Concurrent.Async
import qualified GHC.List as L
import Urist.Landmass
import Urist.Identity
import Urist.Artifact
import Urist.ArtisticForm
import Urist.Entity
import Urist.EntityPopulation
import Urist.HistoricalEra
import Urist.HistoricalEventCollection
import Urist.HistoricalEventRelationshipSupplement
import Urist.HistoricalEventRelationship
import Urist.HistoricalEvent
import Urist.HistoricalFigure
import Urist.River
import Urist.WorldConstruction
import Urist.WrittenContent

data DfWorld

parse :: (Breadcrumbs :> es, IOE :> es, HasCallStack) => FilePath -> FilePath -> Eff es (Either (CallStack, Text) ())
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

parseWorld :: (Breadcrumbs :> es, IOE :> es, Error Text :> es) => Map ByteString (Map (Either Int Text) [XML.Node]) -> Eff es ()
parseWorld m = do
  res <- runStateShared (S.empty @Text) $ do
    let lookupOrThrow :: (Error Text :> es) => ByteString -> Eff es (Map (Either Int Text) NodeMap)
        lookupOrThrow n = case n `M.lookup` m of
          Nothing -> throwError $ "Could not find node " <> show n
          Just r -> pure $ M.map toItemMap r
        parseItems :: (Breadcrumbs :> es, IOE :> es, State (Set Text) :> es, Error Text :> es)
          => ByteString -> Eff (Error Text : State NodeMap : {- Concurrent :-} es) a -> Eff es (Map (Either Int Text) a)
        parseItems n f = do
          r <- lookupOrThrow n
          collection <- M.mapMaybe id <$> mapM {- runConcurrent $ mapConcurrently -} (takeNodeMapAs n f) r
          let logStr = "Finished parsing" <> show n <> " with a total of " <> show (M.size collection) <> " things"
          addAnnotation logStr
          print logStr
          pure collection
    altName <- asText =<< getGlobalNode "altname" m
    name <- asText =<< getGlobalNode "name" m
    --regions <- parseItems "regions" parseRegion
    --undergroundRegions <- parseItems "underground_regions" parseUndergroundRegion
    --sites <- parseItems "sites" parseSite
    --landmasses <- parseItems "landmasses" parseLandmass
    --danceForms <- parseItems "dance_forms" (parseArtisticForm (Proxy @'Dance))
    --poeticForms <- parseItems "poetic_forms" (parseArtisticForm (Proxy @'Poetic))
    --musicalForms <- parseItems "musical_forms"  (parseArtisticForm (Proxy @'Musical))
    --writtenContents <- parseItems "written_contents" parseWrittenContent
    --historicalEras <- parseItems "historical_eras" parseHistoricalEra
    --rivers <- parseItems "rivers" parseRiver
    --worldConstructions <- parseItems "world_constructions" parseWorldConstruction
    --entityPopulations <- parseItems "entity_populations" parseEntityPopulation
    identities <- parseItems "identities" parseIdentity
{-
    artifacts <- parseItems "artifacts" parseArtifact


    entities <- parseItems "entities" parseEntity

    historicalEventCollections <- parseItems "historical_event_collections" parseHistoricalEventCollection
    historicalEventRelationshipSupplements <- parseItems "historical_event_relationship_supplements" parseHistoricalEventRelationshipSupplement
    historicalEventRelationships <- parseItems "historical_event_relationships" parseHistoricalEventRelationship
    historicalEvents <- parseItems "historical_events" parseHistoricalEvent
    historicalFigures <- parseItems "historical_figures" parseHistoricalFigure

  -}
    seq m pass
  case res of
    (_, errs) -> mapM_ print errs

getGlobalNode :: Error Text :> es => ByteString -> Map ByteString (Map (Either Int Text) [XML.Node]) -> Eff es XML.Node
getGlobalNode n m = case M.lookup n m >>= M.lookup (Left 0) >>= viaNonEmpty head of
  Nothing -> throwError $ "Could not find global node " <> show n <> show (M.lookup n m)
  Just r -> pure r

buildIntermediateWorldRep :: XML.Node -> XML.Node -> Either Text (Map ByteString (Map (Either Int Text) [XML.Node]))
buildIntermediateWorldRep vanilla plus =
  let -- given some node, we want to make it into a id-indexed map
      nodeToIdMap :: XML.Node -> Either Text [(ByteString, Map (Either Int Text) [XML.Node])]
      nodeToIdMap n = do
        traverse (\childNode -> do
          traceShow (XML.name childNode) pass
          case XML.name childNode of
            x
              | x `elem` ["creature_raw", "name", "altname"] ->
                pure (XML.name childNode, M.singleton (Left 0) [childNode])
            x
              | x `elem` ["historical_eras", "rivers"] -> (do
                    byIds <- mapM (eitherNodeTextId "name") $ XML.children childNode
                    pure (XML.name childNode, M.fromList $ map (first Right) byIds) )
            x
              | x `elem` ["historical_event_relationships", "historical_event_relationship_supplements"] -> do
                    byIds <- mapM (eitherNodeId "event") $ XML.children childNode
                    pure (XML.name childNode, M.fromList $ map (first Left) byIds)
            _ -> do
                    byIds <- mapM (eitherNodeId "id") $ XML.children childNode
                    pure (XML.name childNode, M.fromList $ map (first Left) byIds) ) (XML.children n)
  in do
    -- the children of vmap/pmap are the artifacts, dance_forms, etc.
    v <- M.fromList <$> nodeToIdMap vanilla
    p <- M.fromList <$> nodeToIdMap plus
    pure $ M.unionWith (M.unionWith (<>)) v p

{-}
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
