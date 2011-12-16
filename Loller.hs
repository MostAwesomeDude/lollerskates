module Loller where

data Item = Empty
          | BootsOfSpeed
          | DoransRing
          | RegrowthPendant
    deriving (Enum, Eq, Ord, Show)

data Stats = Stats { price :: Int
                   , health :: Int
                   , hRegen :: Int
                   , mana :: Int
                   , mRegen :: Int
                   , aD :: Int
                   , aP :: Int }
    deriving (Show)

stats :: Stats
stats = Stats 0 0 0 0 0 0 0

statsFor :: Item -> Stats
statsFor BootsOfSpeed = stats { price = 300 }
statsFor DoransRing = stats { price = 475, health = 100, mRegen = 7, aP = 15 }
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
    in Stats p h hr m mr ad ap

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
