module Loller where

import Data.List
import Data.Ord

data Item = Empty
          | AmplifyingTome
          | BFSword
          | BlastingWand
          | BootsOfSpeed
          | BrawlersGloves
          | ChainVest
          | ClothArmor
          | Dagger
          | DoransRing
          | NeedlesslyLargeRod
          | RecurveBow
          | RegrowthPendant
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
                   , aS :: Float
                   , critChance :: Int
                   , movementSpeed :: Int }
    deriving (Show)

stats :: Stats
stats = Stats 0 0 0 0 0 0 0 0 0 1.0 0 0

statsFor :: Item -> Stats
statsFor AmplifyingTome = stats { price = 435, aP = 20 }
statsFor BFSword = stats { price = 1650, aD = 45 }
statsFor BlastingWand = stats { price = 860, aP = 40 }
statsFor BootsOfSpeed = stats { price = 300, movementSpeed = 1 }
statsFor BrawlersGloves = stats { price = 400, critChance = 8 }
statsFor ChainVest = stats { price = 700, armor = 45 }
statsFor ClothArmor = stats { price = 300, armor = 18 }
statsFor Dagger = stats { price = 420, aS = 1.15 }
statsFor DoransRing = stats { price = 475, health = 100, mRegen = 7, aP = 15 }
statsFor NeedlesslyLargeRod = stats { price = 1600, aP = 80 }
statsFor RecurveBow = stats { price = 1050, aS = 1.4 }
statsFor RegrowthPendant = stats { price = 435, hRegen = 15 }
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
    as = aS first * aS second
    cc = critChance first + critChance second
    ms = max (movementSpeed first) (movementSpeed second)
    in Stats p h hr m mr ad ap a mres as cc ms

withGuard :: ([Item] -> Bool) -> [Item] -> [Item]
withGuard f is = if f is then is else []

twoEmptySlots :: [[Item]]
twoEmptySlots = do
    one <- [succ Empty ..]
    two <- [one ..]
    three <- [two ..]
    four <- [three ..]
    return [Empty, Empty, one, two, three, four]

oneEmptySlot :: [[Item]]
oneEmptySlot = do
    one <- [succ Empty ..]
    two <- [one ..]
    three <- [two ..]
    four <- [three ..]
    five <- [four ..]
    return [Empty, one, two, three, four, five]

fullBuild :: [[Item]]
fullBuild = do
    one <- [succ Empty ..]
    two <- [one ..]
    three <- [two ..]
    four <- [three ..]
    five <- [four ..]
    six <- [five ..]
    return [one, two, three, four, five, six]

isBoots :: Item -> Bool
isBoots BootsOfSpeed = True
isBoots _ = False

hasBoots :: [Item] -> Bool
hasBoots = any isBoots

withBoots :: [Item] -> [Item]
withBoots = withGuard hasBoots

-- | Sum up the stats for a build.
buildStats :: [Item] -> Stats
buildStats = foldr (addStats . statsFor) stats

-- | Find the maximum build in a given attribute.
maxBuild :: Ord a => (Stats -> a) -> [[Item]] -> [Item]
maxBuild attr is = maximumBy (comparing $ attr . buildStats) is
