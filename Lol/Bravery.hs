module Lol.Bravery where

import Prelude

import Control.Monad
import Control.Monad.Random
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
randomBuild :: MonadRandom m => m Build
randomBuild = liftM (sort . take 6 . nub) getRandoms

bootsBuild :: MonadRandom m => m Build
bootsBuild = do
    -- Remember, randomR takes a *range*, this is the A-Z of boots
    boots <- getRandomR (BerserkersGreaves, SorcerorsShoes)
    raw <- getRandoms
    let filtered = sort . take 5 . nub . filter (not . isBoots) $ raw
    return $ boots : filtered

randomSpells :: MonadRandom m => m (Spell, Spell)
randomSpells = let packer (a1:a2:[]) = (a1, a2)
    in liftM (packer . sort . take 2 . nub) getRandoms

makeBravery :: MonadRandom m => m Bravery
makeBravery = do
    champ <- getRandom
    build <- randomBuild
    spells <- randomSpells
    ability <- getRandom
    return $ Bravery champ spells build ability

makeBootsBravery :: MonadRandom m => m Bravery
makeBootsBravery = do
    champ <- getRandom
    build <- bootsBuild
    spells <- randomSpells
    ability <- getRandom
    return $ Bravery champ spells build ability
