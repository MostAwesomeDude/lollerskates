module Lol.Stats where

import Prelude

import Lol.Items

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

stats :: Stats
stats = Stats 0 0 0 0 0 0 0 0 0 0 0 0 0 0

statsFor :: Item -> Stats
statsFor AmplifyingTome = stats { price = 435, abilityPower = 20 }
statsFor AvariceBlade = stats { price = 750, criticalChance = 12 }
statsFor BFSword = stats { price = 1650, attackDamage = 45 }
statsFor BansheesVeil =
    stats { price = 2715, health = 375, mana = 375, magicResist = 50 }
statsFor BlastingWand = stats { price = 860, abilityPower = 40 }
statsFor BootsOfSpeed = stats { price = 300, movementSpeed = 1 }
statsFor BrawlersGloves = stats { price = 400, criticalChance = 8 }
statsFor CatalystTheProtector =
    stats { price = 1325, health = 290, mana = 325 }
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
statsFor ExecutionersCalling =
    stats { price = 1350, lifeSteal = 18, criticalChance = 15 }
statsFor FaerieCharm = stats { price = 180, manaRegen = 3 }
statsFor FrozenMallet =
    stats { price = 3250, attackDamage = 20, health = 700 }
statsFor GiantsBelt = stats { price = 1110, health = 430 }
statsFor HauntingGuise =
    stats { price = 1485, health = 200, abilityPower = 25 }
statsFor HeartOfGold = stats { price = 825, health = 250 }
statsFor Hexdrinker =
    stats { price = 1800, attackDamage = 35, magicResist = 30 }
statsFor KagesLuckyPick = stats { price = 765, abilityPower = 25 }
-- XXX this needs to also have +7% movement speed
statsFor LichBane = stats { price = 3470
                          , abilityPower = 80
                          , magicResist = 30
                          , mana = 350 }
statsFor LongSword = stats { price = 410, attackDamage = 10 }
-- XXX actually 7.2
statsFor ManaManipulator = stats { price = 475, manaRegen = 7 }
statsFor MekiPendant = stats { price = 390, manaRegen = 7 }
statsFor NeedlesslyLargeRod = stats { price = 1600, abilityPower = 80 }
statsFor NegatronCloak = stats { price = 740, magicResist = 48 }
statsFor NullMagicMantle = stats { price = 400, magicResist = 24 }
statsFor Phage = stats { price = 1315, attackDamage = 18, health = 225 }
statsFor Pickaxe = stats { price = 975, attackDamage = 25 }
statsFor RabadonsDeathcap = stats { price = 3600, abilityPower = 140 }
statsFor RanduinsOmen =
    stats { price = 3075, health = 350, armor = 75, healthRegen = 25 }
statsFor RecurveBow = stats { price = 1050, attackSpeed = 40 }
statsFor RegrowthPendant = stats { price = 435, healthRegen = 15 }
statsFor RejuvenationBead = stats { price = 250, healthRegen = 8 }
statsFor RodOfAges =
    stats { price = 3035, abilityPower = 60, health = 450, mana = 525 }
statsFor RubyCrystal = stats { price = 475, health = 180 }
statsFor RylaisCrystalScepter =
    stats { price = 3105, abilityPower = 80, health = 500 }
statsFor SapphireCrystal = stats { price = 400, mana = 200 }
statsFor Sheen = stats { price = 1260, abilityPower = 25, mana = 250 }
-- XXX also +12% movement speed
statsFor TrinityForce = stats { price = 4070
                              , abilityPower = 30
                              , attackDamage = 30
                              , attackSpeed = 30
                              , criticalChance = 15
                              , health = 250
                              , mana = 250 }
statsFor VampiricScepter = stats { price = 450, lifeSteal = 12 }
statsFor WardensMail = stats { price = 1350, armor = 50, healthRegen = 20 }
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

type Comparator = Stats -> Float