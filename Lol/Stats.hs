module Lol.Stats where

import Prelude

import Control.Monad.ST
import Data.Lens.Template
import qualified Data.Map as M
import Data.Maybe
import Data.STRef

import Lol.Champs
import Lol.Items
import Lol.Maths
import Lol.Stats.Champs
import Lol.Stats.Items
import Lol.Stats.Types

data ChampStats = ChampStats { _cCoreStats :: CoreStats
                             , _cExtendedStats :: ExtendedStats }
    deriving (Show)

$( makeLens ''ChampStats )

data ItemStats = ItemStats { _iPrice :: Price
                           , _iCoreStats :: CoreStats
                           , _iExtendedStats :: ExtendedStats }
    deriving (Show)

$( makeLens ''ItemStats )

addCS :: CoreStats -> CoreStats -> CoreStats
addCS first second = let
    h = _csHealth first + _csHealth second
    m = _csMana first + _csMana second
    ad = _csAttackDamage first + _csAttackDamage second
    as = _csAttackSpeed first + _csAttackSpeed second
    r = _csRange first + _csRange second
    hr = _csHealthRegen first + _csHealthRegen second
    mr = _csManaRegen first + _csManaRegen second
    a = _csArmor first + _csArmor second
    mres = _csMagicResist first + _csMagicResist second
    ms = max (_csMovementSpeed first) (_csMovementSpeed second)
    in CoreStats h m ad as r hr mr a mres ms

csAtLevel :: Level -> CoreStats -> CoreStats
csAtLevel level (CoreStats a b c d e f g h i j) =
    let l = fromIntegral level
    in CoreStats (a*l) (b*l) (c*l) (d*l) (e*l) (f*l) (g*l) (h*l) (i*l) (j*l)

addES :: ExtendedStats -> ExtendedStats -> ExtendedStats
addES first second = let
    ap = _esAbilityPower first + _esAbilityPower second
    steal = _esLifeSteal first + _esLifeSteal second
    vamp = _esSpellVamp first + _esSpellVamp second
    cc = _esCriticalChance first + _esCriticalChance second
    bms = _esBonusMovementSpeed first + _esBonusMovementSpeed second
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
