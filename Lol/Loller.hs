module Lol.Loller where

-- Yesod hides the Prelude. We want it back.
import Prelude

import Data.List
import qualified Data.Map as Map
import Data.Ord

import Lol.FD
import Lol.Items
import Lol.Stats

-- | Turn any accessor into a comparator which considers per-gold worth of an
--   item, by turning any field into field-per-gold.
--   This was the stupid ((/) `on` realToFrac) (f stats) (price stats) before,
--   but now it's tuned to catch division-by-zero NaNs.
worth :: Comparator -> Comparator
worth f stats =
    let p = price stats in case p of
        0 -> 0
        _ -> realToFrac (f stats) / realToFrac p

attributeFilters :: Map.Map String Comparator
attributeFilters = Map.fromList [ ("abilitypower", abilityPower)
                                , ("armor", armor)
                                , ("attackdamage", attackDamage)
                                , ("attackspeed", attackSpeed)
                                , ("criticalchance", criticalChance)
                                , ("health", health)
                                , ("healthregen", healthRegen)
                                , ("lifesteal", lifeSteal)
                                , ("magicresist", magicResist)
                                , ("mana", mana)
                                , ("manaregen", manaRegen)
                                , ("movementspeed", movementSpeed)
                                , ("price", toEnum . price)
                                , ("spellvamp", spellVamp)
                                -- Economic variants: The most bang for your
                                -- buck.
                                -- ]
                                , ("eabilitypower", worth abilityPower)
                                , ("earmor", worth armor)
                                , ("eattackdamage", worth attackDamage)
                                , ("eattackspeed", worth attackSpeed)
                                , ("ecriticalchance", worth criticalChance)
                                , ("ehealth", worth health)
                                , ("ehealthregen", worth healthRegen)
                                , ("elifesteal", worth lifeSteal)
                                , ("emagicresist", worth magicResist)
                                , ("emana", worth mana)
                                , ("emanaregen", worth manaRegen)
                                , ("emovementspeed", worth movementSpeed)
                                , ("espellvamp", worth spellVamp) ]

builds :: [[Item]] -> FD s [FDVar s]
builds = mapM newVar

-- | A default build constraint: Any item, in any slot, but ordered such that
--   iteration should not yield very many repeated combinations.
defaultBuilds :: FD s [FDVar s]
defaultBuilds = do
    build <- builds $ replicate 6 [Empty ..]
    -- orderedIn build
    return build

withEmptySlot :: [FDVar s] -> FD s ()
withEmptySlot build = head build `hasValue` Empty

withVariety :: [FDVar s] -> FD s ()
-- withVariety = orderedEx
withVariety = allDifferent

-- | The 'maximumBy' function takes a comparison function and a list
--   and returns the greatest element of the list by the comparison function.
--   The list must be finite and non-empty.
--   This variant uses foldl1' instead of foldl1, making it consume lists in
--   constant space.
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ [] = error "Loller.maximumBy': empty list"
maximumBy' cmp xs = foldl1' maxBy xs
    where
        maxBy x y = case cmp x y of
                    GT -> x
                    _ -> y

-- | Find the best item in a given attribute.
bestItem :: Comparator -> [Item] -> Item
bestItem attr = maximumBy' (comparing $ attr . itemStats)

-- | Find the maximum build in a given attribute.
maxBuild :: Comparator -> [Build] -> Build
maxBuild attr = maximumBy' (comparing $ attr . buildStats)
