module Lol.Stats where

import Prelude hiding ((.))

import Control.Category
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.State.Lazy
import Data.Lens.Common
import Data.Lens.Lazy
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

-- Imperatively generate the statistics for a given champion.
makeChampStats :: Champ -> Level -> [Item] -> ChampStats
makeChampStats champ level items = execState pipeline (ChampStats base es)
    where
        pipeline :: State ChampStats ()
        pipeline = do
            -- We're going to do this one field at a time. A handful of fields
            -- require exactly zero magic, though, which is nice. To start
            -- out, the core base stats have been populated, and now we're
            -- going to add in the per-level stats.
            focus cCoreStats $ do
                csHealth += (csHealth ^$ perlevel) * flevel
                csHealthRegen += (csHealthRegen ^$ perlevel) * flevel
                csMana += (csMana ^$ perlevel) * flevel
                csManaRegen += (csManaRegen ^$ perlevel) * flevel
                csAttackDamage += (csAttackDamage ^$ perlevel) * flevel
                -- XXX this isn't right.
                csAttackSpeed += (csAttackSpeed ^$ perlevel) * flevel
                csArmor += (csArmor ^$ perlevel) * flevel
                csMagicResist += (csMagicResist ^$ perlevel) * flevel
                -- Movement speed would go here, but no champions have
                -- movement speed per-level stacks.
                -- Quirk: Tristana is the only champion who gains range per
                -- level. 9 per level, not including level 1.
                when (champ == Tristana) (void $ csRange += 9 * (flevel - 1))
            -- Now grab all of the stats from the items, and add them in.
            focus cCoreStats $ do
                csHealth += sum (map (csHealth ^$) itemcorestats)
                csHealthRegen += sum (map (csHealthRegen ^$) itemcorestats)
                csMana += sum (map (csMana ^$) itemcorestats)
                csManaRegen += sum (map (csManaRegen ^$) itemcorestats)
                csAttackDamage += sum (map (csAttackDamage ^$) itemcorestats)
                -- XXX not even close.
                csAttackSpeed += sum (map (csAttackSpeed ^$) itemcorestats)
                csArmor += sum (map (csArmor ^$) itemcorestats)
                csMagicResist += sum (map (csMagicResist ^$) itemcorestats)
                -- Movement speed is done later. Range isn't provided by any
                -- item.
            -- Clamp movement speed.
            ces <- access cExtendedStats
            cCoreStats %= flip finalizeMovementSpeed ces
            return ()
        base :: CoreStats
        base = champBaseStats M.! champ
        perlevel :: CoreStats
        perlevel = champLevelStats M.! champ
        flevel :: Float
        flevel = realToFrac level
        itemcorestats :: [CoreStats]
        itemcorestats = map (getL iCoreStats . itemStats) items
        itemextendedstats :: [ExtendedStats]
        itemextendedstats = map (getL iExtendedStats . itemStats) items
