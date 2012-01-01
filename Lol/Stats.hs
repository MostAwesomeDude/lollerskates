module Lol.Stats where

import Prelude

import Control.Monad.ST
import qualified Data.Map as M
import Data.Maybe
import Data.STRef

import Lol.Champs
import Lol.Items
import Lol.Maths
import Lol.Stats.Champs
import Lol.Stats.Items
import Lol.Stats.Types

data ChampStats = ChampStats { cCoreStats :: CoreStats
                             , cExtendedStats :: ExtendedStats }
    deriving (Show)
data ItemStats = ItemStats { iPrice :: Price
                           , iCoreStats :: CoreStats
                           , iExtendedStats :: ExtendedStats }
    deriving (Show)

type Comparator = ItemStats -> Float

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

csAtLevel :: Level -> CoreStats -> CoreStats
csAtLevel level (CoreStats a b c d e f g h i j) =
    let l = fromIntegral level
    in CoreStats (a*l) (b*l) (c*l) (d*l) (e*l) (f*l) (g*l) (h*l) (i*l) (j*l)

addES :: ExtendedStats -> ExtendedStats -> ExtendedStats
addES first second = let
    ap = esAbilityPower first + esAbilityPower second
    steal = esLifeSteal first + esLifeSteal second
    vamp = esSpellVamp first + esSpellVamp second
    cc = esCriticalChance first + esCriticalChance second
    bms = esBonusMovementSpeed first + esBonusMovementSpeed second
    in ExtendedStats ap steal vamp cc bms

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

-- | Apply a build to a champion.
applyBuild :: Build -> ChampStats -> ChampStats
applyBuild b (ChampStats ccs ces) =
    let (ItemStats _ ics ies) = buildStats b
    in ChampStats (addCS ccs ics) (addES ces ies)

-- | Finalize a champion build statistics.
finalizeStats :: ChampStats -> ChampStats
finalizeStats (ChampStats ccs ces) = runST $ do
    core <- newSTRef ccs
    -- Apply movement speed.
    modifySTRef core $ flip finalizeMovementSpeed ces
    -- All done!
    core' <- readSTRef core
    return $ ChampStats core' ces

-- Accessors.

price :: ItemStats -> Price
price = iPrice

armor :: Comparator
armor = csArmor . iCoreStats
attackDamage :: Comparator
attackDamage = csAttackDamage . iCoreStats
attackSpeed :: Comparator
attackSpeed = csAttackSpeed . iCoreStats
health :: Comparator
health = csHealth . iCoreStats
healthRegen :: Comparator
healthRegen = csHealthRegen . iCoreStats
magicResist :: Comparator
magicResist = csMagicResist . iCoreStats
mana :: Comparator
mana = csMana . iCoreStats
manaRegen :: Comparator
manaRegen = csManaRegen . iCoreStats
movementSpeed :: Comparator
movementSpeed = csMovementSpeed . iCoreStats

abilityPower :: Comparator
abilityPower = esAbilityPower . iExtendedStats
criticalChance :: Comparator
criticalChance = esCriticalChance . iExtendedStats
lifeSteal :: Comparator
lifeSteal = esLifeSteal . iExtendedStats
spellVamp :: Comparator
spellVamp = esSpellVamp . iExtendedStats
