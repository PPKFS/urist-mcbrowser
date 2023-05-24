module Urist.HistoricalFigure where

import Solitude
import Effectful.Error.Static
import qualified Xeno.DOM as XML
import Urist.ParseHelpers
import Urist.Id

data HistoricalFigure = HistoricalFigure

parseHistoricalFigure :: (Error Text :> es) => XML.Node -> Eff es HistoricalFigure
parseHistoricalFigure n = do
  let m = nodeChildrenToMap n
  historicalFigureId  <- parseId HistoricalFigureId m
  name <- getNodeText "name" m
  race <- getNodeText "race" m >>= parseRace
  caste <- getNodeText "caste" m >>= parseCaste
  appeared <- getNodeInt "appeared" m
  birthYear <- getNodeInt "birth_year" m
  birthSeconds <- getNodeInt "birth_seconds72" m
  deathYear <- getNodeInt "death_year" m
  deathSeconds <- error ""
  pure HistoricalFigure

parseCaste :: Text -> Eff es a0
parseCaste = error ""

parseRace :: Text -> Eff es a1
parseRace = error ""

{-
<historical_figure>
		<id>0</id>
		<name>faci swelteredsilvers the wealthy</name>
		<race>DRAGON</race>
		<caste>FEMALE</caste>
		<appeared>1</appeared>
		<birth_year>-177</birth_year>
		<birth_seconds72>-1</birth_seconds72>
		<death_year>-1</death_year>
		<death_seconds72>-1</death_seconds72>
		<associated_type>STANDARD</associated_type>
		<hf_link>
			<link_type>child</link_type>
			<hfid>52775</hfid>
		</hf_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>1011</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>799</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>956</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>835</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>1086</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>761</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>844</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>843</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>1368</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>1424</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>1409</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>3839</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>873</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>1462</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>3380</entity_id>
		</entity_link>
		<entity_link>
			<link_type>enemy</link_type>
			<entity_id>1876</entity_id>
		</entity_link>
		<sphere>fire</sphere>
		<sphere>wealth</sphere>
	</historical_figure>
-}