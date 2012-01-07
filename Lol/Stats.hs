{-# LANGUAGE TemplateHaskell #-}

module Lol.Stats where

import Prelude hiding ((.))

import Control.Category
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Lens.Common
import Data.Lens.Lazy
import Data.Lens.Template
import qualified Data.Map as M
import Data.Maybe

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
                csArmor += (csArmor ^$ perlevel) * flevel
                csMagicResist += (csMagicResist ^$ perlevel) * flevel
                -- Movement speed would go here, but no champions have
                -- movement speed per-level stacks.
                -- Quirk: Tristana is the only champion who gains range per
                -- level. 9 per level, not including level 1.
                when (champ == Tristana) (void $ csRange += 9 * (flevel - 1))
            -- Attack speed per level is calculated as bonus attack speed
            -- instead of core attack speed, and doesn't provide a stack at
            -- level 1.
            esBonusAttackSpeed . cExtendedStats ~=
                (csAttackSpeed ^$ perlevel) * (flevel - 1)
            -- Now grab all of the stats from the items, and add them in.
            focus cCoreStats $ do
                csHealth += sum (map (csHealth ^$) icstats)
                csHealthRegen += sum (map (csHealthRegen ^$) icstats)
                csMana += sum (map (csMana ^$) icstats)
                csManaRegen += sum (map (csManaRegen ^$) icstats)
                csAttackDamage += sum (map (csAttackDamage ^$) icstats)
                -- Attack speed would go here, but no items increase base
                -- attack speed.
                csArmor += sum (map (csArmor ^$) icstats)
                csMagicResist += sum (map (csMagicResist ^$) icstats)
                -- Movement speed is done later. Range isn't provided by any
                -- item.
            focus cExtendedStats $ do
                esAbilityPower += sum (map (esAbilityPower ^$) iestats)
                esLifeSteal += sum (map (esLifeSteal ^$) iestats)
                esSpellVamp += sum (map (esSpellVamp ^$) iestats)
                esCriticalChance += sum (map (esCriticalChance ^$) iestats)
                esBonusAttackSpeed += sum (map (esBonusAttackSpeed ^$) iestats)
                esBonusMovementSpeed += sum (map (esBonusMovementSpeed ^$) iestats)
            -- Clamp attack speed. Requires extended stats.
            ces <- access cExtendedStats
            cCoreStats %= flip finalizeAttackSpeed ces
            -- Clamp movement speed. Requires extended stats.
            cCoreStats %= flip finalizeMovementSpeed ces
            -- Clamp critical chance.
            esCriticalChance . cExtendedStats %= finalizeCriticalChance
            return ()
        base :: CoreStats
        base = champBaseStats M.! champ
        perlevel :: CoreStats
        perlevel = champLevelStats M.! champ
        flevel :: Float
        flevel = realToFrac level
        icstats :: [CoreStats]
        icstats = map (getL iCoreStats . itemStats) items
        iestats :: [ExtendedStats]
        iestats = map (getL iExtendedStats . itemStats) items

-- | Calculate the DPS for a given champion.
--   Presumably, the stats have already been calculated from a build, but raw
--   champion stats are fine too.
--   The formula that is used is damage times attacks per second, modulated by
--   the odds of a critical strike: (AD + AD * CC) * AS
dps :: ChampStats -> Float
dps cs =
    let ad = csAttackDamage . cCoreStats ^$ cs
        as = csAttackSpeed . cCoreStats ^$ cs
        cc = esCriticalChance . cExtendedStats ^$ cs
    in (ad + ad * cc) * as
