module Urist.Identity where

import Solitude hiding (Identity)
import Effectful.Error.Static
import Urist.ParseHelpers
import Urist.Id

data Identity = Identity
-- so this is where it starts to become guesswork
-- https://github.com/DFHack/df-structures/blob/master/df.history.xml#LL939C1-L939C22
-- https://github.com/DFHack/scripts/blob/a26e72b6e1da16d6fcaadc71518314fb8aaae193/exportlegends.lua#L292

-- histfig_id is many things
-- Seen on adventurer assuming an identity for reasons unknown
-- name='HidingCurse' comment="Inferred from Units.cpp after examining code using 'unk_4c'"
-- Impersonating' comment="Seen where primeval creatures impersonate 'real' gods in modded game"
-- E.g. of demonic overlords. Can be used by adventurers to gain sway over them
-- Claim a new official identity, seen when religious appointments are received

-- so far I've only seen histfig be with a birth year of -1000000001..

-- and nemesis_id is "Infiltration Identity" - it seems to be that the historical figure (nemesis id) fools the entity
-- into believing this fake identity
-- todo: are the dates here ever different to the actual character's life?

parseIdentity :: (Error Text :> es, State NodeMap :> es) => Eff es Identity
parseIdentity = pure Identity