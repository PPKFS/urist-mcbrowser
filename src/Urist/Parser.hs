module Urist.Parser where

import Solitude
import qualified Xeno.DOM as XML
import Breadcrumbs
import Urist.Region
import Urist.UndergroundRegion
import Urist.ParseHelpers
import Urist.Site
import Urist.Artifact (parseArtifact)

data DfWorld

parse :: (Breadcrumbs :> es, (State (Set Text) :> es), IOE :> es) => FilePath -> Eff es ()
parse fp = withSpan' "XML parsing" "df_world" $ do
  f <- liftIO $ readFileBS fp
  addAnnotation "Read file"
  let dom = XML.parse f
  addAnnotation "DOM parsed"
  case dom of
    Left er -> error $ show er
    Right n -> parseDom n

parseDom :: (State (Set Text) :> es) => XML.Node -> Eff es () -- [XML.Node]
parseDom root = withExpectedNode' "df_world" root $ do
  let pairs = childrenToAssocList root
  regions <- fromListOfEntries "regions" parseRegion pairs
  undergroundRegions <- fromListOfEntries "underground_regions" parseUndergroundRegion pairs
  sites <- fromListOfEntries "sites" parseSite pairs
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
 "historical_eras" -> parseHistoricalEras
 "written_contents" -> parseWrittenContents
 "poetic_forms" -> parsePoeticForms
 "musical_forms" -> parseMusicalForms
 "dance_forms" -> parseDanceForms
 x -> failOnUnknownNodeType x
-}


failOnUnknownNodeType :: Text -> XML.Node -> XML.Node
failOnUnknownNodeType x = const (error $ "could not parse node type " <> x)
