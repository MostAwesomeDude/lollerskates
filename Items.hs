module Items where

data Item = Empty
          | AbyssalScepter
          | AegisOfTheLegion
          | AmplifyingTome
          | ArchangelsStaff
          | AtmasImpaler
          | AvariceBlade
          | BFSword
          | BerserkersGreaves
          | BilgewaterCutlass
          | BlastingWand
          | BootsOfMobility
          | BootsOfSpeed
          | BootsOfSwiftness
          | BrawlersGloves
          | CatalystTheProtector
          | ChainVest
          | ChaliceOfHarmony
          | CloakAndDagger
          | CloakOfAgility
          | ClothArmor
          | Dagger
          | DeathfireGrasp
          | DoransBlade
          | DoransRing
          | DoransShield
          | EleisasMiracle
          | EmblemOfValor
          | ExecutionersCalling
          | FaerieCharm
          | FiendishCodex
          | ForceOfNature
          | FrozenHeart
          | FrozenMallet
          | GiantsBelt
          | GlacialShroud
          | GuardianAngel
          | GuinsoosRageblade
          | HauntingGuise
          | HeartOfGold
          | Hexdrinker
          | HextechGunblade
          | HextechRevolver
          | InfinityEdge
          | IonianBootsOfLucidity
          | KagesLuckyPick
          | Kindlegem
          | LastWhisper
          | LichBane
          | LongSword
          | Malady
          | ManaManipulator
          | Manamune
          | MekiPendant
          | MercurysTreads
          | MoonflairSpellblade
          | MorellosEvilTome
          | NashorsTooth
          | NeedlesslyLargeRod
          | NegatronCloak
          | NinjaTabi
          | NullMagicMantle
          | Phage
          | PhantomDancer
          | PhilosophersStone
          | Pickaxe
          | QuicksilverSash
          | RabadonsDeathcap
          | RanduinsOmen
          | RecurveBow
          | RegrowthPendant
          | RejuvenationBead
          | RodOfAges
          | RubyCrystal
          | RylaisCrystalScepter
          | SapphireCrystal
          | Sheen
          | ShurelyasReverie
          | SorcerorsShoes
          | SoulShroud
          | SpiritVisage
          | StarksFervor
          | Stinger
          | SunfireCape
          | SwordOfTheDivine
          | SwordOfTheOccult
          | TearOftheGoddess
          | TheBlackCleaver
          | TheBrutalizer
          | Thornmail
          | Tiamat
          | TrinityForce
          | VampiricScepter
          | VoidStaff
          | WardensMail
          | WillOfTheAncients
          | WitsEnd
          | YoumuusGhostblade
          | Zeal
          | ZhonyasHourglass
    deriving (Enum, Eq, Ord, Read, Show)

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
statsFor HeartOfGold = stats { price = 825, health = 250 }
statsFor KagesLuckyPick = stats { price = 765, abilityPower = 25 }
statsFor LongSword = stats { price = 410, attackDamage = 10 }
statsFor MekiPendant = stats { price = 390, manaRegen = 7 }
statsFor NeedlesslyLargeRod = stats { price = 1600, abilityPower = 80 }
statsFor NegatronCloak = stats { price = 740, magicResist = 48 }
statsFor NullMagicMantle = stats { price = 400, magicResist = 24 }
statsFor Pickaxe = stats { price = 975, attackDamage = 25 }
statsFor RanduinsOmen =
    stats { price = 3075, health = 350, armor = 75, healthRegen = 25 }
statsFor RecurveBow = stats { price = 1050, attackSpeed = 40 }
statsFor RegrowthPendant = stats { price = 435, healthRegen = 15 }
statsFor RejuvenationBead = stats { price = 250, healthRegen = 8 }
statsFor RubyCrystal = stats { price = 475, health = 180 }
statsFor SapphireCrystal = stats { price = 400, mana = 200 }
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