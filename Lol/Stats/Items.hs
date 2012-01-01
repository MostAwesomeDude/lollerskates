module Lol.Stats.Items where

import qualified Data.Map as M

import Lol.Items
import Lol.Stats.Types

cs :: CoreStats
cs = CoreStats 0 0 0 0.0 0 0 0.0 0 0 0

es :: ExtendedStats
es = ExtendedStats 0 0 0 0 0

-- Tables for item statistics. Not even close to ideal, but ATM I can't think
-- of a better way to do it.

-- | Table of item prices.
itemPrice :: M.Map Item Price
itemPrice = M.fromList [ (Empty, 0)
                       , (AbyssalScepter, 2650)
                       , (AegisOfTheLegion, 1925)
                       , (AmplifyingTome, 435)
                       , (ArchangelsStaff, 2855)
                       , (AtmasImpaler, 2355)
                       , (AvariceBlade, 750)
                       , (BFSword, 1650)
                       , (BansheesVeil, 2715)
                       , (BerserkersGreaves, 920)
                       , (BilgewaterCutlass, 1825)
                       , (BlastingWand, 860)
                       , (BootsOfMobility, 1000)
                       , (BootsOfSpeed, 300)
                       , (BootsOfSwiftness, 1000)
                       , (BrawlersGloves, 400)
                       , (CatalystTheProtector, 1325)
                       , (ChainVest, 700)
                       , (ChaliceOfHarmony, 890)
                       , (ClothArmor, 300)
                       , (CloakAndDagger, 1450)
                       , (CloakOfAgility, 830)
                       , (Dagger, 420)
                       , (DeathfireGrasp, 2610)
                       , (DoransBlade, 475)
                       , (DoransRing, 475)
                       , (DoransShield, 475)
                       , (EleisasMiracle, 1300)
                       , (EmblemOfValor, 800)
                       , (ExecutionersCalling, 1350)
                       , (FaerieCharm, 180)
                       , (FiendishCodex, 1245)
                       , (ForceOfNature, 2610)
                       , (FrozenHeart, 2775)
                       , (FrozenMallet, 3250)
                       , (GiantsBelt, 1110)
                       , (GlacialShroud, 1525)
                       , (GuardianAngel, 2600)
                       , (GuinsoosRageblade, 2235)
                       , (HauntingGuise, 1485)
                       , (HeartOfGold, 825)
                       , (Hexdrinker, 1800)
                       , (HextechGunblade, 3625)
                       , (HextechRevolver, 1200)
                       , (IonianBootsOfLucidity, 1050)
                       , (InfinityEdge, 3830)
                       , (KagesLuckyPick, 765)
                       , (Kindlegem, 850)
                       , (LastWhisper, 2290)
                       , (LichBane, 3470)
                       , (LongSword, 410)
                       , (Malady, 1825)
                       , (ManaManipulator, 475)
                       , (Manamune, 2110)
                       , (MekiPendant, 390)
                       , (MercurysTreads, 1200)
                       , (MoonflairSpellblade, 1200)
                       , (MorellosEvilTome, 2350)
                       , (NashorsTooth, 2885)
                       , (NeedlesslyLargeRod, 1600)
                       , (NegatronCloak, 740)
                       , (NinjaTabi, 850)
                       , (NullMagicMantle, 400)
                       , (Phage, 1315)
                       , (PhantomDancer, 2845)
                       , (PhilosophersStone, 800)
                       , (Pickaxe, 975)
                       , (QuicksilverSash, 1440)
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
                       , (SoulShroud, 2285)
                       , (SpiritVisage, 1550)
                       , (StarksFervor, 2550)
                       , (Stinger, 1140)
                       , (SunfireCape, 2610)
                       , (SwordOfTheDivine, 1970)
                       , (SwordOfTheOccult, 1369)
                       , (TearOfTheGoddess, 995)
                       , (Tiamat, 2070)
                       , (TheBlackCleaver, 2865)
                       , (TheBrutalizer, 1337)
                       , (Thornmail, 2000)
                       , (TrinityForce, 4070)
                       , (VampiricScepter, 450)
                       , (VoidStaff, 2295)
                       , (WardensMail, 1350)
                       , (WillOfTheAncients, 2100)
                       , (WitsEnd, 2000)
                       , (YoumuusGhostblade, 2687)
                       , (Zeal, 1195)
                       , (ZhonyasHourglass, 3100) ]

-- | Table of core item stats.
itemCoreStats :: M.Map Item CoreStats
itemCoreStats =
    M.fromList [ (AbyssalScepter, cs { csMagicResist = 57 })
               , (AegisOfTheLegion,
                  cs { csHealth = 270, csArmor = 18, csMagicResist = 24 })
               , (ArchangelsStaff, cs { csMana = 400, csManaRegen = 25 })
               , (AtmasImpaler, cs { csArmor = 45 })
               , (BFSword, cs { csAttackDamage = 45 })
               , (BansheesVeil,
                  cs { csHealth = 375, csMana = 375, csMagicResist = 50 })
               , (BilgewaterCutlass, cs { csAttackDamage = 35 })
               , (CatalystTheProtector, cs { csHealth = 290, csMana = 325 })
               , (ChainVest, cs { csArmor = 45 })
               , (ChaliceOfHarmony, cs { csManaRegen = 7.5, csMagicResist = 30 })
               , (ClothArmor, cs { csArmor = 18 })
               , (DeathfireGrasp, cs { csManaRegen = 10 })
               , (DoransBlade, cs { csHealth = 100, csAttackDamage = 10 })
               , (DoransRing, cs { csHealth = 100, csManaRegen = 7 })
               , (DoransShield,
                  cs { csHealth = 120 , csHealthRegen = 120 , csArmor = 10 })
               , (EleisasMiracle, cs { csHealthRegen = 25, csManaRegen = 20 })
               , (FaerieCharm, cs { csManaRegen = 3 })
               , (FiendishCodex, cs { csManaRegen = 7 })
               , (ForceOfNature, cs { csHealthRegen = 40, csMagicResist = 76 })
               , (FrozenHeart, cs { csMana = 500, csArmor = 99 })
               , (FrozenMallet, cs { csHealth = 700, csAttackDamage = 20 })
               , (GiantsBelt, cs { csHealth = 430 })
               , (GlacialShroud, cs { csMana = 425, csArmor = 45 })
               , (GuardianAngel, cs { csArmor = 68, csMagicResist = 38 })
               , (GuinsoosRageblade, cs { csAttackDamage = 35 })
               , (HauntingGuise, cs { csHealth = 200 })
               , (HeartOfGold, cs { csHealth = 250 })
               , (Hexdrinker, cs { csAttackDamage = 35, csMagicResist = 30 })
               , (HextechRevolver, cs { csAttackDamage = 40 })
               , (InfinityEdge, cs { csAttackDamage = 80 })
               , (Kindlegem, cs { csHealth = 200 })
               , (LastWhisper, cs { csAttackDamage = 40 })
               , (LichBane, cs { csMana = 350, csMagicResist = 30 })
               , (LongSword, cs { csAttackDamage = 10 })
               , (ManaManipulator, cs { csManaRegen = 7.2 })
               , (Manamune,
                  cs { csMana = 350, csAttackDamage = 20, csManaRegen = 7 })
               , (MekiPendant, cs { csManaRegen = 7 })
               , (MercurysTreads, cs { csMagicResist = 25 })
               , (MorellosEvilTome, cs { csManaRegen = 12 })
               , (NashorsTooth, cs { csManaRegen = 10 })
               , (NegatronCloak, cs { csMagicResist = 48 })
               , (NinjaTabi, cs { csArmor = 25 })
               , (NullMagicMantle, cs { csMagicResist = 24 })
               , (Phage, cs { csHealth = 225, csAttackDamage = 18 })
               , (PhilosophersStone, cs { csHealthRegen = 18, csManaRegen = 8 })
               , (Pickaxe, cs { csAttackDamage = 25 })
               , (QuicksilverSash, cs { csMagicResist = 56 })
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
               , (SoulShroud, cs { csHealth = 520 })
               , (SpiritVisage, cs { csHealth = 250, csMagicResist = 30 })
               , (SunfireCape, cs { csHealth = 450, csArmor = 45 })
               , (SwordOfTheOccult, cs { csAttackDamage = 10 })
               , (TearOfTheGoddess, cs { csMana = 350, csManaRegen = 7 })
               , (TheBlackCleaver, cs { csAttackDamage = 55 })
               , (TheBrutalizer, cs { csAttackDamage = 25 })
               , (Thornmail, cs { csArmor = 100 })
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
               , (AtmasImpaler, es { esCriticalChance = 18 })
               , (AvariceBlade, es { esCriticalChance = 12 })
               , (BilgewaterCutlass, es { esLifeSteal = 15 })
               , (BlastingWand, es { esAbilityPower = 40 })
               , (BrawlersGloves, es { esCriticalChance = 8 })
               , (CloakAndDagger, es { esCriticalChance = 20 })
               , (CloakOfAgility, es { esCriticalChance = 18 })
               , (DeathfireGrasp, es { esAbilityPower = 60 })
               , (DoransBlade, es { esLifeSteal = 3 })
               , (DoransRing, es { esAbilityPower = 15 })
               , (EmblemOfValor, es { esLifeSteal = 17 })
               , (ExecutionersCalling,
                  es { esLifeSteal = 18, esCriticalChance = 15 })
               , (FiendishCodex, es { esAbilityPower = 30 })
               , (GuinsoosRageblade, es { esAbilityPower = 45 })
               , (HauntingGuise, es { esAbilityPower = 25 })
               , (HextechGunblade,
                  es { esAbilityPower = 70, esLifeSteal = 15 })
               , (HextechRevolver, es { esAbilityPower = 40 })
               , (InfinityEdge, es { esCriticalChance = 25 })
               , (KagesLuckyPick, es { esAbilityPower = 25 })
               , (LichBane, es { esAbilityPower = 80 })
               , (Malady, es { esAbilityPower = 25 })
               , (MoonflairSpellblade, es { esAbilityPower = 50 })
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
               , (WillOfTheAncients, es { esAbilityPower = 50 })
               , (YoumuusGhostblade, es { esCriticalChance = 15 })
               , (Zeal, es { esCriticalChance = 10 })
               , (ZhonyasHourglass, es { esAbilityPower = 100 }) ]
