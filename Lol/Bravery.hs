module Lol.Bravery where

import Prelude

import Data.List
import System.Random

import Lol.Champs
import Lol.Items
import Lol.Helpers
import Lol.Spells

data Ability = Q | W | E
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Random Ability where
    randomR = boundedEnumRandomR
    random = boundedEnumRandom

data Bravery = Bravery { bChamp :: Champ
                       , bSpells :: (Spell, Spell)
                       , bBuild :: Build
                       , bAbility :: Ability }
    deriving (Show)

-- Yes, I know that this nubs, but quadratic time doesn't hurt that bad when
-- there's only 6 items being taken.
randomBuild :: RandomGen g => g -> Build
randomBuild = sort . take 6 . nub . randoms

randomSpells :: RandomGen g => g -> (Spell, Spell)
randomSpells = let packer (a1:a2:as) = (a1, a2)
    in packer . sort . take 2 . nub . randoms

bootsBuild :: RandomGen g => g -> Build
bootsBuild gen =
    -- Remember, randomR takes a *range*, this is the A-Z of boots
    let (bootItem, gen') = randomR (BerserkersGreaves, SorcerorsShoes) gen
        otherItems = nub $ filter (not . isBoots) $ randoms gen'
    in (bootItem :) $ sort $ take 5 otherItems

-- This function could be terser if Random were automatic on Enums. :T
randomAbility :: RandomGen g => g -> Ability
randomAbility = fst . random

makeBravery :: IO Bravery
makeBravery = do
    g <- getStdGen
    (champ, g) <- return $ random g
    return $ Bravery champ (randomSpells g) (randomBuild g) (randomAbility g)

makeBootsBravery :: IO Bravery
makeBootsBravery = do
    g <- getStdGen
    (champ, g) <- return $ random g
    return $ Bravery champ (randomSpells g) (bootsBuild g) (randomAbility g)
