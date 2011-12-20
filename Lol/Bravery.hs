module Lol.Bravery where

import Prelude

import Data.List
import System.Random

import Lol.Items

data Bravery = Bravery { bBuild :: Build }
    deriving (Show)

randomBuild :: RandomGen g => g -> Build
randomBuild = sort . take 6 . randoms

bootsBuild :: RandomGen g => g -> Build
bootsBuild gen =
    let (bootItem, gen') = randomR (BerserkersGreaves, SorcerorsShoes) gen
        otherItems = filter (not . isBoots) $ randoms gen'
    in (bootItem :) $ sort $ take 5 otherItems

makeBravery :: IO Bravery
makeBravery = do
    randomgen <- getStdGen
    return $ Bravery (randomBuild randomgen)

makeBootsBravery :: IO Bravery
makeBootsBravery = do
    randomgen <- getStdGen
    return $ Bravery (bootsBuild randomgen)
