module Urist.Parser where

import Solitude
import qualified Xeno.DOM as XML
data DfWorld

parse :: Eff es DfWorld
parse = do
  f <- liftIO $ readFileBS "region1-00250-01-01-legends.xml"
  case XML.parse f of
    Left er -> error $ show er
    Right n -> pure $ parseDom n

parseDom :: XML.Node -> [XML.Node]
parseDom root = do
  -- insert verification that indeed we are on the top node..
  map (\n -> parseWorldItem (decodeUtf8 $ XML.name n) n) (XML.children root)

parseWorldItem :: Text -> XML.Node -> WorldItem
parseWorldItem = \case
 "regions" -> parseRegions
 "underground_regions" -> parseUndergroundRegions
 "sites" -> parseSites
 "world_constructions" -> parseWorldConstructions
 "artifacts" -> parseArtifacts
 "historical_figures" -> parseHistoricalFigures
 "entity_populations" -> parseEntityPopulations
 "entities" -> parseEntities
 "historical_events" -> parseHistoricalEvents
 "historical_event_collections" -> parseHistoricalEventCollections
 "historical_eras" -> parseHistoricalEras
 "written_contents" -> parseWrittenContents
 "poetic_forms" -> parsePoeticForms
 "musical_forms" -> parseMusicalForms
 "dance_forms" -> parseDanceForms
 x -> failOnUnknownNodeType x

parseRegions :: XML.Node -> XML.Node
parseRegions = withExpectedNode "regions" $ forEachChild "region" parseRegion

withExpectedNode :: Text -> (XML.Node -> a) -> XML.Node -> a
withExpectedNode = _

forEachChild :: t0 -> t1 -> a0
forEachChild = _



failOnUnknownNodeType :: Text -> XML.Node -> XML.Node
failOnUnknownNodeType x = const (error $ "could not parse node type " <> x)
