module Loller where

import Data.List
import qualified Data.Map as Map
import Data.Ord

import FD

data Item = Empty
          | AmplifyingTome
          | BFSword
          | BlastingWand
          | BootsOfSpeed
          | BrawlersGloves
          | ChainVest
          | CloakOfAgility
          | ClothArmor
          | Dagger
          | DoransBlade
          | DoransRing
          | DoransShield
          | FaerieCharm
          | GiantsBelt
          | LongSword
          | MekiPendant
          | NeedlesslyLargeRod
          | NegatronCloak
          | NullMagicMantle
          | Pickaxe
          | RecurveBow
          | RegrowthPendant
          | RejuvenationBead
          | RubyCrystal
          | SapphireCrystal
          | VampiricScepter
    deriving (Enum, Eq, Ord, Read, Show)

type Build = [Item]

data Stats = Stats { price :: Int
                   , health :: Int
                   , healthRegen :: Int
                   , mana :: Int
                   , manaRegen :: Int
                   , attackDamage :: Int
                   , abilityPower :: Int
                   , armor :: Int
                   , magicResist :: Int
                   , lifeSteal :: Int
                   , spellVamp :: Int
                   , attackSpeed :: Int
                   , criticalChance :: Int
                   , movementSpeed :: Int }
    deriving (Show)

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

stats :: Stats
stats = Stats 0 0 0 0 0 0 0 0 0 0 0 0 0 0

statsFor :: Item -> Stats
statsFor AmplifyingTome = stats { price = 435, abilityPower = 20 }
statsFor BFSword = stats { price = 1650, attackDamage = 45 }
statsFor BlastingWand = stats { price = 860, abilityPower = 40 }
statsFor BootsOfSpeed = stats { price = 300, movementSpeed = 1 }
statsFor BrawlersGloves = stats { price = 400, criticalChance = 8 }
statsFor ChainVest = stats { price = 700, armor = 45 }
statsFor ClothArmor = stats { price = 300, armor = 18 }
statsFor CloakOfAgility = stats { price = 830, criticalChance = 18 }
statsFor Dagger = stats { price = 420, attackSpeed = 15 }
statsFor DoransBlade =
    stats { price = 475, health = 100, attackDamage = 10, lifeSteal = 3 }
statsFor DoransRing = stats
    { price = 475, health = 100, manaRegen = 7, abilityPower = 15 }
statsFor DoransShield =
    stats { price = 475, health = 120, healthRegen = 120, armor = 10 }
statsFor FaerieCharm = stats { price = 180, manaRegen = 3 }
statsFor GiantsBelt = stats { price = 1110, health = 430 }
statsFor LongSword = stats { price = 410, attackDamage = 10 }
statsFor MekiPendant = stats { price = 390, manaRegen = 7 }
statsFor NeedlesslyLargeRod = stats { price = 1600, abilityPower = 80 }
statsFor NegatronCloak = stats { price = 740, magicResist = 48 }
statsFor NullMagicMantle = stats { price = 400, magicResist = 24 }
statsFor Pickaxe = stats { price = 975, attackDamage = 25 }
statsFor RecurveBow = stats { price = 1050, attackSpeed = 40 }
statsFor RegrowthPendant = stats { price = 435, healthRegen = 15 }
statsFor RejuvenationBead = stats { price = 250, healthRegen = 8 }
statsFor RubyCrystal = stats { price = 475, health = 180 }
statsFor SapphireCrystal = stats { price = 400, mana = 200 }
statsFor VampiricScepter = stats { price = 450, lifeSteal = 12 }
statsFor _ = stats

addStats :: Stats -> Stats -> Stats
addStats first second = let
    p = price first + price second
    h = health first + health second
    hr = healthRegen first + healthRegen second
    m = mana first + mana second
    mr = manaRegen first + manaRegen second
    ad = attackDamage first + attackDamage second
    ap = abilityPower first + abilityPower second
    a = armor first + armor second
    mres = magicResist first + magicResist second
    steal = lifeSteal first + lifeSteal second
    vamp = spellVamp first + spellVamp second
    as = attackSpeed first + attackSpeed second
    cc = criticalChance first + criticalChance second
    ms = max (movementSpeed first) (movementSpeed second)
    in Stats p h hr m mr ad ap a mres steal vamp as cc ms

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

-- | Find the maximum build in a given attribute.
maxBuild :: Ord a => (Stats -> a) -> [Build] -> Build
maxBuild attr = maximumBy' (comparing $ attr . buildStats)
