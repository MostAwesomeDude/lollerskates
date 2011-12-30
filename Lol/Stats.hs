module Lol.Stats where

import Prelude

import qualified Data.Map as M
import Data.Maybe

import Lol.Items
import Lol.Stats.Items
import Lol.Stats.Types

data ChampStats = ChampStats CoreStats ExtendedStats
    deriving (Show)
data ItemStats = ItemStats Price CoreStats ExtendedStats
    deriving (Show)

type Comparator = ItemStats -> Float

itemStats :: Item -> ItemStats
itemStats i =
    let price = fromMaybe 0 $ M.lookup i itemPrice
        core = fromMaybe cs $ M.lookup i itemCoreStats
        extended = fromMaybe es $ M.lookup i itemExtendedStats
    in ItemStats price core extended

addCS :: CoreStats -> CoreStats -> CoreStats
addCS first second = let
    h = csHealth first + csHealth second
    m = csMana first + csMana second
    ad = csAttackDamage first + csAttackDamage second
    as = csAttackSpeed first + csAttackSpeed second
    r = csRange first + csRange second
    hr = csHealthRegen first + csHealthRegen second
    mr = csManaRegen first + csManaRegen second
    a = csArmor first + csArmor second
    mres = csMagicResist first + csMagicResist second
    ms = max (csMovementSpeed first) (csMovementSpeed second)
    in CoreStats h m ad as r hr mr a mres ms

addES :: ExtendedStats -> ExtendedStats -> ExtendedStats
addES first second = let
    ap = esAbilityPower first + esAbilityPower second
    steal = esLifeSteal first + esLifeSteal second
    vamp = esSpellVamp first + esSpellVamp second
    cc = esCriticalChance first + esCriticalChance second
    in ExtendedStats ap steal vamp cc

addStats :: ItemStats -> ItemStats -> ItemStats
addStats (ItemStats px csx esx) (ItemStats py csy esy) =
    ItemStats (px + py) (addCS csx csy) (addES esx esy)

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
