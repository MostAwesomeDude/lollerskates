module Lol.Stats where

import Prelude

import qualified Data.Map as M
import Data.Maybe

import Lol.Items

type Price = Int

data CoreStats = CoreStats { csHealth :: Float
                           , csMana :: Float
                           , csAttackDamage :: Float
                           , csAttackSpeed :: Float
                           , csRange :: Float
                           , csHealthRegen :: Float
                           , csManaRegen :: Float
                           , csArmor :: Float
                           , csMagicResist :: Float
                           , csMovementSpeed :: Float }
    deriving (Show)

data ExtendedStats = ExtendedStats { esAbilityPower :: Float
                                   , esLifeSteal :: Float
                                   , esSpellVamp :: Float
                                   , esCriticalChance :: Float }
    deriving (Show)

data ChampStats = ChampStats CoreStats ExtendedStats
    deriving (Show)
data ItemStats = ItemStats Price CoreStats ExtendedStats
    deriving (Show)

type Comparator = ItemStats -> Float

cs :: CoreStats
cs = CoreStats 0 0 0 0.0 0 0 0.0 0 0 0

es :: ExtendedStats
es = ExtendedStats 0 0 0 0

-- Tables for item statistics. Not even close to ideal, but ATM I can't think
-- of a better way to do it.

-- | Table of item prices.
itemPrice :: M.Map Item Int
itemPrice = M.fromList [ (AbyssalScepter, 2650)
                       , (AegisOfTheLegion, 1925)
                       , (AmplifyingTome, 435)
                       , (ArchangelsStaff, 2855)
                       , (AvariceBlade, 750)
                       , (BFSword, 1650)
                       , (BansheesVeil, 2715)
                       , (BerserkersGreaves, 920)
                       , (BlastingWand, 860)
                       , (BootsOfMobility, 1000)
                       , (BootsOfSpeed, 300)
                       , (BootsOfSwiftness, 1000)
                       , (BrawlersGloves, 400)
                       , (CatalystTheProtector, 1325)
                       , (ChainVest, 700)
                       , (ClothArmor, 300)
                       , (CloakAndDagger, 1450)
                       , (CloakOfAgility, 830)
                       , (Dagger, 420)
                       , (DeathfireGrasp, 2610)
                       , (DoransBlade, 475)
                       , (DoransRing, 475)
                       , (DoransShield, 475)
                       , (ExecutionersCalling, 1350)
                       , (FaerieCharm, 180)
                       , (FiendishCodex, 1245)
                       , (FrozenMallet, 3250)
                       , (GiantsBelt, 1110)
                       , (HauntingGuise, 1485)
                       , (HeartOfGold, 825)
                       , (Hexdrinker, 1800)
                       , (IonianBootsOfLucidity, 1050)
                       , (KagesLuckyPick, 765)
                       , (LichBane, 3470)
                       , (LongSword, 410)
                       , (Malady, 1825)
                       , (ManaManipulator, 475)
                       , (MekiPendant, 390)
                       , (MercurysTreads, 1200)
                       , (MorellosEvilTome, 2350)
                       , (NashorsTooth, 2885)
                       , (NeedlesslyLargeRod, 1600)
                       , (NegatronCloak, 740)
                       , (NinjaTabi, 850)
                       , (NullMagicMantle, 400)
                       , (Phage, 1315)
                       , (PhantomDancer, 2845)
                       , (Pickaxe, 975)
                       , (RabadonsDeathcap, 3600)
                       , (RanduinsOmen, 3075)
                       , (RecurveBow, 1050)
                       , (RegrowthPendant, 435)
                       , (RejuvenationBead, 250)
                       , (RodOfAges, 3035)
                       , (RubyCrystal, 475)
                       , (RylaisCrystalScepter, 3105)
                       , (SapphireCrystal, 400)
                       , (Sheen, 1260)
                       , (ShurelyasReverie, 2200)
                       , (SorcerorsShoes, 1100)
                       , (Stinger, 1140)
                       , (Tiamat, 2070)
                       , (TheBlackCleaver, 2865)
                       , (TrinityForce, 4070)
                       , (VampiricScepter, 450)
                       , (VoidStaff, 2295)
                       , (WardensMail, 1350)
                       , (WitsEnd, 2000)
                       , (YoumuusGhostblade, 2687)
                       , (Zeal, 1195)
                       , (ZhonyasHourglass, 3100) ]

-- | AtmasImpaler
-- | BilgewaterCutlass
-- | ChaliceOfHarmony
-- | EleisasMiracle
-- | EmblemOfValor
-- | ForceOfNature
-- | FrozenHeart
-- | FrozenMallet
-- | GlacialShroud
-- | GuardianAngel
-- | GuinsoosRageblade
-- | HextechGunblade
-- | HextechRevolver
-- | InfinityEdge
-- | Kindlegem
-- | LastWhisper
-- | Manamune
-- | MoonflairSpellblade
-- | PhilosophersStone
-- | QuicksilverSash
-- | SoulShroud
-- | SpiritVisage
-- | StarksFervor
-- | SunfireCape
-- | SwordOfTheDivine
-- | SwordOfTheOccult
-- | TearOfTheGoddess
-- | TheBrutalizer
-- | Thornmail
-- | WillOfTheAncients

-- | Table of core item stats.
itemCoreStats :: M.Map Item CoreStats
itemCoreStats =
    M.fromList [ (AbyssalScepter, cs { csMagicResist = 57 })
               , (AegisOfTheLegion,
                  cs { csHealth = 270, csArmor = 18, csMagicResist = 24 })
               , (ArchangelsStaff, cs { csMana = 400, csManaRegen = 25 })
               , (BFSword, cs { csAttackDamage = 45 })
               , (BansheesVeil,
                  cs { csHealth = 375, csMana = 375, csMagicResist = 50 })
               , (CatalystTheProtector, cs { csHealth = 290, csMana = 325 })
               , (ChainVest, cs { csArmor = 45 })
               , (ClothArmor, cs { csArmor = 18 })
               , (DeathfireGrasp, cs { csManaRegen = 10 })
               , (DoransBlade, cs { csHealth = 100, csAttackDamage = 10 })
               , (DoransRing, cs { csHealth = 100, csManaRegen = 7 })
               , (DoransShield,
                  cs { csHealth = 120 , csHealthRegen = 120 , csArmor = 10 })
               , (FaerieCharm, cs { csManaRegen = 3 })
               , (FiendishCodex, cs { csManaRegen = 7 })
               , (FrozenMallet, cs { csHealth = 700, csAttackDamage = 20 })
               , (GiantsBelt, cs { csHealth = 430 })
               , (HauntingGuise, cs { csHealth = 200 })
               , (HeartOfGold, cs { csHealth = 250 })
               , (Hexdrinker, cs { csAttackDamage = 35, csMagicResist = 30 })
               , (LichBane, cs { csMana = 350, csMagicResist = 30 })
               , (LongSword, cs { csAttackDamage = 10 })
               , (ManaManipulator, cs { csManaRegen = 7.2 })
               , (MekiPendant, cs { csManaRegen = 7 })
               , (MercurysTreads, cs { csMagicResist = 25 })
               , (MorellosEvilTome, cs { csManaRegen = 12 })
               , (NashorsTooth, cs { csManaRegen = 10 })
               , (NegatronCloak, cs { csMagicResist = 48 })
               , (NinjaTabi, cs { csArmor = 25 })
               , (NullMagicMantle, cs { csMagicResist = 24 })
               , (Phage, cs { csHealth = 225, csAttackDamage = 18 })
               , (Pickaxe, cs { csAttackDamage = 25 })
               , (RanduinsOmen,
                  cs { csHealth = 350, csHealthRegen = 25, csArmor = 75 })
               , (RegrowthPendant, cs { csHealthRegen = 15 })
               , (RejuvenationBead, cs { csHealthRegen = 8 })
               , (RodOfAges, cs { csHealth = 450, csMana = 525 })
               , (RubyCrystal, cs { csHealth = 180 })
               , (RylaisCrystalScepter, cs { csHealth = 500 })
               , (SapphireCrystal, cs { csMana = 200 })
               , (Sheen, cs { csMana = 250 })
               , (ShurelyasReverie,
                 cs { csHealth = 330, csHealthRegen = 30, csManaRegen = 15 })
               , (TheBlackCleaver, cs { csAttackDamage = 55 })
               , (Tiamat, cs { csHealthRegen = 15
                             , csManaRegen = 5
                             , csAttackDamage = 50 })
               , (TrinityForce,
                  cs { csHealth = 250, csMana = 250, csAttackDamage = 30 })
               , (WardensMail, cs { csHealthRegen = 20, csArmor = 50 })
               , (WitsEnd, cs { csMagicResist = 30 })
               , (YoumuusGhostblade, cs { csAttackDamage = 30 })
               , (ZhonyasHourglass, cs { csArmor = 50 }) ]

-- | Table of extended item stats.
itemExtendedStats :: M.Map Item ExtendedStats
itemExtendedStats =
    M.fromList [ (AbyssalScepter, es { esAbilityPower = 70 })
               , (AmplifyingTome, es { esAbilityPower = 20 })
               , (ArchangelsStaff, es { esAbilityPower = 45 })
               , (AvariceBlade, es { esCriticalChance = 12 })
               , (BlastingWand, es { esAbilityPower = 40 })
               , (BrawlersGloves, es { esCriticalChance = 8 })
               , (CloakAndDagger, es { esCriticalChance = 20 })
               , (CloakOfAgility, es { esCriticalChance = 18 })
               , (DeathfireGrasp, es { esAbilityPower = 60 })
               , (DoransBlade, es { esLifeSteal = 3 })
               , (DoransRing, es { esAbilityPower = 15})
               , (ExecutionersCalling,
                  es { esLifeSteal = 18, esCriticalChance = 15 })
               , (FiendishCodex, es { esAbilityPower = 30 })
               , (HauntingGuise, es { esAbilityPower = 25 })
               , (KagesLuckyPick, es { esAbilityPower = 25 })
               , (LichBane, es { esAbilityPower = 80 })
               , (Malady, es { esAbilityPower = 25 })
               , (MorellosEvilTome, es { esAbilityPower = 75 })
               , (NeedlesslyLargeRod, es { esAbilityPower = 80 })
               , (PhantomDancer, es { esCriticalChance = 30 })
               , (RabadonsDeathcap, es { esAbilityPower = 140 })
               , (RodOfAges, es { esAbilityPower = 60 })
               , (RylaisCrystalScepter, es { esAbilityPower = 80 })
               , (Sheen, es { esAbilityPower = 25 })
               , (TrinityForce,
                  es { esAbilityPower = 30, esCriticalChance = 15 })
               , (VampiricScepter, es { esLifeSteal = 12 })
               , (VoidStaff, es { esAbilityPower = 70 })
               , (YoumuusGhostblade, es { esCriticalChance = 15 })
               , (Zeal, es { esCriticalChance = 10 })
               , (ZhonyasHourglass, es { esAbilityPower = 100 }) ]

itemStats :: Item -> ItemStats
itemStats i =
    let price = itemPrice M.! i
        cs = fromMaybe cs $ M.lookup i itemCoreStats
        es = fromMaybe es $ M.lookup i itemExtendedStats
    in ItemStats price cs es

addCS :: CoreStats -> CoreStats -> CoreStats
addCS first second = let
    h = csHealth first + csHealth second
    m = csMana first + csMana second
    ad = csAttackDamage first + csAttackDamage second
    as = csAttackSpeed first + csAttackSpeed second
    r = csRange first + csRange second
    hr = csHealthRegen first + csHealthRegen second
    mr = csManaRegen first + csManaRegen second
    a = csArmor first + csArmor second
    mres = csMagicResist first + csMagicResist second
    ms = max (csMovementSpeed first) (csMovementSpeed second)
    in CoreStats h m ad as r hr mr a mres ms

addES :: ExtendedStats -> ExtendedStats -> ExtendedStats
addES first second = let
    ap = esAbilityPower first + esAbilityPower second
    steal = esLifeSteal first + esLifeSteal second
    vamp = esSpellVamp first + esSpellVamp second
    cc = esCriticalChance first + esCriticalChance second
    in ExtendedStats ap steal vamp cc

addStats :: ItemStats -> ItemStats -> ItemStats
addStats (ItemStats px csx esx) (ItemStats py csy esy) =
    ItemStats (px + py) (addCS csx csy) (addES esx esy)

-- | Sum up the stats for a build.
buildStats :: Build -> ItemStats
buildStats = foldr1 addStats . map itemStats

-- Accessors.

price (ItemStats p _ _) = p
getCoreStats (ItemStats _ cs _) = cs
getExtendedStats (ItemStats _ _ es) = es

armor = csArmor . getCoreStats
attackDamage = csAttackDamage . getCoreStats
attackSpeed = csAttackSpeed . getCoreStats
health = csHealth . getCoreStats
healthRegen = csHealthRegen . getCoreStats
magicResist = csMagicResist . getCoreStats
mana = csMana . getCoreStats
manaRegen = csManaRegen . getCoreStats
movementSpeed = csMovementSpeed . getCoreStats

criticalChance = esCriticalChance . getExtendedStats
lifeSteal = esLifeSteal . getExtendedStats
spellVamp = esSpellVamp . getExtendedStats
abilityPower = esAbilityPower . getExtendedStats
