module Loller where

import Data.List
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
    deriving (Enum, Eq, Ord, Show)

data Stats = Stats { price :: Int
                   , health :: Int
                   , hRegen :: Int
                   , mana :: Int
                   , mRegen :: Int
                   , aD :: Int
                   , aP :: Int
                   , armor :: Int
                   , mResist :: Int
                   , lifeSteal :: Int
                   , spellVamp :: Int
                   , aS :: Int
                   , critChance :: Int
                   , movementSpeed :: Int }
    deriving (Show)

stats :: Stats
stats = Stats 0 0 0 0 0 0 0 0 0 0 0 0 0 0

statsFor :: Item -> Stats
statsFor AmplifyingTome = stats { price = 435, aP = 20 }
statsFor BFSword = stats { price = 1650, aD = 45 }
statsFor BlastingWand = stats { price = 860, aP = 40 }
statsFor BootsOfSpeed = stats { price = 300, movementSpeed = 1 }
statsFor BrawlersGloves = stats { price = 400, critChance = 8 }
statsFor ChainVest = stats { price = 700, armor = 45 }
statsFor ClothArmor = stats { price = 300, armor = 18 }
statsFor CloakOfAgility = stats { price = 830, critChance = 18 }
statsFor Dagger = stats { price = 420, aS = 15 }
statsFor DoransBlade =
    stats { price = 475, health = 100, aD = 10, lifeSteal = 3 }
statsFor DoransRing = stats { price = 475, health = 100, mRegen = 7, aP = 15 }
statsFor DoransShield =
    stats { price = 475, health = 120, hRegen = 120, armor = 10 }
statsFor FaerieCharm = stats { price = 180, mRegen = 3 }
statsFor GiantsBelt = stats { price = 1110, health = 430 }
statsFor LongSword = stats { price = 410, aD = 10 }
statsFor MekiPendant = stats { price = 390, mRegen = 7 }
statsFor NeedlesslyLargeRod = stats { price = 1600, aP = 80 }
statsFor NegatronCloak = stats { price = 740, mResist = 48 }
statsFor NullMagicMantle = stats { price = 400, mResist = 24 }
statsFor Pickaxe = stats { price = 975, aD = 25 }
statsFor RecurveBow = stats { price = 1050, aS = 40 }
statsFor RegrowthPendant = stats { price = 435, hRegen = 15 }
statsFor RejuvenationBead = stats { price = 250, hRegen = 8 }
statsFor RubyCrystal = stats { price = 475, health = 180 }
statsFor SapphireCrystal = stats { price = 400, mana = 200 }
statsFor VampiricScepter = stats { price = 450, lifeSteal = 12 }
statsFor _ = stats

addStats :: Stats -> Stats -> Stats
addStats first second = let
    p = price first + price second
    h = health first + health second
    hr = hRegen first + hRegen second
    m = mana first + mana second
    mr = mRegen first + mRegen second
    ad = aD first + aD second
    ap = aP first + aP second
    a = armor first + armor second
    mres = mResist first + mResist second
    steal = lifeSteal first + lifeSteal second
    vamp = spellVamp first + spellVamp second
    as = aS first + aS second
    cc = critChance first + critChance second
    ms = max (movementSpeed first) (movementSpeed second)
    in Stats p h hr m mr ad ap a mres steal vamp as cc ms

builds :: FD s [FDVar s]
builds = do
    build <- newVars 6 [Empty ..]
    orderedIn build
    return build

withEmptySlot :: [FDVar s] -> FD s ()
withEmptySlot build = head build `hasValue` Empty

withVariety :: [FDVar s] -> FD s ()
withVariety build = orderedEx build

-- | Sum up the stats for a build.
buildStats :: [Item] -> Stats
buildStats = foldr (addStats . statsFor) stats

-- | Find the maximum build in a given attribute.
maxBuild :: Ord a => (Stats -> a) -> [[Item]] -> [Item]
maxBuild attr = maximumBy (comparing $ attr . buildStats)
