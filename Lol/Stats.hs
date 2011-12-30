module Lol.Stats where

import Prelude

import qualified Data.Map as M
import Data.Maybe

import Lol.Champs
import Lol.Items
import Lol.Stats.Champs
import Lol.Stats.Items
import Lol.Stats.Types

data ChampStats = ChampStats CoreStats ExtendedStats
    deriving (Show)
data ItemStats = ItemStats Price CoreStats ExtendedStats
    deriving (Show)

type Comparator = ItemStats -> Float

addCSWith :: (Float -> Float -> Float) -> CoreStats -> CoreStats -> CoreStats
addCSWith op first second = let
    h = csHealth first `op` csHealth second
    m = csMana first `op` csMana second
    ad = csAttackDamage first `op` csAttackDamage second
    as = csAttackSpeed first `op` csAttackSpeed second
    r = csRange first `op` csRange second
    hr = csHealthRegen first `op` csHealthRegen second
    mr = csManaRegen first `op` csManaRegen second
    a = csArmor first `op` csArmor second
    mres = csMagicResist first `op` csMagicResist second
    -- XXX ms bonghits
    ms = max (csMovementSpeed first) (csMovementSpeed second)
    in CoreStats h m ad as r hr mr a mres ms

addCS = addCSWith (+)
mulCS = addCSWith (*)

csAtLevel :: Level -> CoreStats -> CoreStats
csAtLevel level (CoreStats a b c d e f g h i j) =
    let l = fromIntegral level
    in CoreStats (a*l) (b*l) (c*l) (d*l) (e*l) (f*l) (g*l) (h*l) (i*l) (j*l)

addESWith :: (Float -> Float -> Float) -> ExtendedStats -> ExtendedStats -> ExtendedStats
addESWith op first second = let
    ap = esAbilityPower first `op` esAbilityPower second
    steal = esLifeSteal first `op` esLifeSteal second
    vamp = esSpellVamp first `op` esSpellVamp second
    cc = esCriticalChance first `op` esCriticalChance second
    in ExtendedStats ap steal vamp cc

addES = addESWith (+)

itemStats :: Item -> ItemStats
itemStats i =
    let price = fromMaybe 0 $ M.lookup i itemPrice
        core = fromMaybe cs $ M.lookup i itemCoreStats
        extended = fromMaybe es $ M.lookup i itemExtendedStats
    in ItemStats price core extended

addStats :: ItemStats -> ItemStats -> ItemStats
addStats (ItemStats px csx esx) (ItemStats py csy esy) =
    ItemStats (px + py) (addCS csx csy) (addES esx esy)

champStats :: Champ -> Level -> ChampStats
champStats c l =
    let base = champBaseStats M.! c
        levels = csAtLevel l (champLevelStats M.! c)
    in ChampStats (addCS base levels) es

-- | Sum up the stats for a build.
buildStats :: Build -> ItemStats
buildStats = foldr1 addStats . map itemStats

-- Accessors.

price :: ItemStats -> Price
price (ItemStats p _ _) = p
getCoreStats :: ItemStats -> CoreStats
getCoreStats (ItemStats _ cs _) = cs
getExtendedStats :: ItemStats -> ExtendedStats
getExtendedStats (ItemStats _ _ es) = es

armor :: Comparator
armor = csArmor . getCoreStats
attackDamage :: Comparator
attackDamage = csAttackDamage . getCoreStats
attackSpeed :: Comparator
attackSpeed = csAttackSpeed . getCoreStats
health :: Comparator
health = csHealth . getCoreStats
healthRegen :: Comparator
healthRegen = csHealthRegen . getCoreStats
magicResist :: Comparator
magicResist = csMagicResist . getCoreStats
mana :: Comparator
mana = csMana . getCoreStats
manaRegen :: Comparator
manaRegen = csManaRegen . getCoreStats
movementSpeed :: Comparator
movementSpeed = csMovementSpeed . getCoreStats

abilityPower :: Comparator
abilityPower = esAbilityPower . getExtendedStats
criticalChance :: Comparator
criticalChance = esCriticalChance . getExtendedStats
lifeSteal :: Comparator
lifeSteal = esLifeSteal . getExtendedStats
spellVamp :: Comparator
spellVamp = esSpellVamp . getExtendedStats
