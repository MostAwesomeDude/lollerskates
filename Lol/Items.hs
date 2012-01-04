module Lol.Items where

-- Yesod hides our Prelude, so we need to explicitly ask for it.
import Prelude

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import System.Random hiding (split)

import Lol.Helpers

-- | The Item datatype.
--   This type's constructors represent all of the different items available
--   in Lollerskates. The list of constructors is carefully ordered to permit
--   several useful optimizations related to enumeration; in particular,
--   certain items like boots are clustered rather than alphabetical.
data Item = Empty
          -- Boots.
          | BerserkersGreaves
          | BootsOfMobility
          | BootsOfSpeed
          | BootsOfSwiftness
          | IonianBootsOfLucidity
          | MercurysTreads
          | NinjaTabi
          | SorcerorsShoes
          -- General items.
          | AbyssalScepter
          | AegisOfTheLegion
          | AmplifyingTome
          | ArchangelsStaff
          | AtmasImpaler
          | AvariceBlade
          | BFSword
          | BansheesVeil
          | BilgewaterCutlass
          | BlastingWand
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
          | KagesLuckyPick
          | Kindlegem
          | LastWhisper
          | LichBane
          | LongSword
          | Malady
          | ManaManipulator
          | Manamune
          | MekiPendant
          | MoonflairSpellblade
          | MorellosEvilTome
          | NashorsTooth
          | NeedlesslyLargeRod
          | NegatronCloak
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
          | SoulShroud
          | SpiritVisage
          | StarksFervor
          | Stinger
          | SunfireCape
          | SwordOfTheDivine
          | SwordOfTheOccult
          | TearOfTheGoddess
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
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- Since Random doesn't automatically happen on types which are Bounded and
-- Enum, we go ahead and implement a straightforward and non-magical flavor of
-- Random here.
instance Random Item where
    randomR = boundedEnumRandomR
    random = boundedEnumRandom

-- Some item classes.

-- | The different kinds of boots, all in one place.
boots :: [Item]
boots = [ BerserkersGreaves .. SorcerorsShoes ]

isBoots :: Item -> Bool
isBoots = flip elem boots

hasBoots :: [Item] -> Bool
hasBoots = any isBoots

type Build = [Item]

-- | Specialized pretty words for some of the fragments of words in the item
--   list. I was considering just typing out static strings for every single
--   item, but this is actually far less space. Seriously! (Even including
--   this comment!)
prettyWordMap :: M.Map String String
prettyWordMap = M.fromList [ ("Archangels", "Archangel's")
                           , ("Atmas", "Atma's")
                           , ("B", "B.")
                           , ("Banshees", "Banshee's")
                           , ("Berserkers", "Berserker's")
                           , ("Brawlers", "Brawler's")
                           , ("Dorans", "Doran's")
                           , ("Executioners", "Executioner's")
                           , ("F", "F.")
                           , ("Giants", "Giant's")
                           , ("Guinsoos", "Guinsoo's")
                           , ("Kages", "Kage's")
                           , ("Mercurys", "Mercury's")
                           , ("Morellos", "Morello's")
                           , ("Nashors", "Nashor's")
                           , ("Rabadons", "Rabadon's")
                           , ("Randuins", "Randuin's")
                           , ("Rylais", "Rylai's")
                           , ("Shurelyas", "Shurelya's")
                           , ("Sorcerors", "Sorceror's")
                           , ("Starks", "Stark's")
                           , ("Wardens", "Warden's")
                           , ("Wits", "Wit's")
                           , ("Youmuus", "Youmuu's")
                           , ("Zhonyas", "Zhonya's") ]

-- | Pretty-printing service for items, because items should be relatively
--   pretty.
prettyItem :: Item -> String
prettyItem =
    let predicate c = 'A' <= c && c <= 'Z'
        splitter = dropInitBlank $ keepDelimsL $ whenElt predicate
        mapper w = fromMaybe w $ M.lookup w prettyWordMap
    in unwords . map mapper . split splitter . show
