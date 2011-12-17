module Loller where

import Data.List
import qualified Data.Map as Map
import Data.Ord

import FD
import Items

attributeFilters :: Map.Map String (Stats -> Int)
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
                                , ("price", price)
                                , ("spellvamp", spellVamp) ]

type Build = [Item]

builds :: [[Item]] -> FD s [FDVar s]
builds = mapM newVar

-- | A default build constraint: Any item, in any slot, but ordered such that
--   iteration should not yield very many repeated combinations.
defaultBuilds :: FD s [FDVar s]
defaultBuilds = do
    build <- builds $ replicate 6 [Empty ..]
    orderedIn build
    return build

withEmptySlot :: [FDVar s] -> FD s ()
withEmptySlot build = head build `hasValue` Empty

withVariety :: [FDVar s] -> FD s ()
withVariety = orderedEx

-- | Sum up the stats for a build.
buildStats :: Build -> Stats
buildStats = foldr (addStats . statsFor) stats

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
bestItem :: Ord a => (Stats -> a) -> [Item] -> Item
bestItem attr = maximumBy' (comparing $ attr . statsFor)

-- | Find the maximum build in a given attribute.
maxBuild :: Ord a => (Stats -> a) -> [Build] -> Build
maxBuild attr = maximumBy' (comparing $ attr . buildStats)
