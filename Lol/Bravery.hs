module Lol.Bravery where

import Prelude

import Data.List
import System.Random

import Lol.Items

data Bravery = Bravery { bBuild :: Build }
    deriving (Show)

-- Yes, I know that this nubs, but quadratic time doesn't hurt that bad when
-- there's only 6 items being taken.
randomBuild :: RandomGen g => g -> Build
randomBuild = sort . take 6 . nub . randoms

bootsBuild :: RandomGen g => g -> Build
bootsBuild gen =
    -- Remember, randomR takes a *range*, this is the A-Z of boots
    let (bootItem, gen') = randomR (BerserkersGreaves, SorcerorsShoes) gen
        otherItems = nub $ filter (not . isBoots) $ randoms gen'
    in (bootItem :) $ sort $ take 5 otherItems

makeBravery :: IO Bravery
makeBravery = do
    randomgen <- getStdGen
    return $ Bravery (randomBuild randomgen)

makeBootsBravery :: IO Bravery
makeBootsBravery = do
    randomgen <- getStdGen
    return $ Bravery (bootsBuild randomgen)
