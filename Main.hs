module Main where

import Prelude hiding ((.))

import Control.Category
import Data.Lens.Common
import Data.Ord

import Lol.Build
import Lol.Champs
import Lol.Constraints
import Lol.FD
import Lol.Items
import Lol.Stats
import Lol.Stats.Types

improvesDPS :: Item -> Bool
improvesDPS i = let stats = itemStats i
    in any (> 0) $ map (^$ stats) [csAttackDamage . iCoreStats
                                  ,esBonusAttackSpeed . iExtendedStats
                                  ,esCriticalChance . iExtendedStats]

buildList :: [[Item]]
buildList = runFD $ do
    build <- defaultBuilds
    head build `satisfies` isBoots
    mapM_ (flip satisfies $ not . isBoots) (tail build)
    mapM_ (flip lacksValue Empty) (tail build)
    mapM_ (flip satisfies improvesDPS) build
    orderedEx build
    labelling build

createBestBuild :: Build
createBestBuild = let bs = map (makeBuild Tryndamere 18) buildList
    in maximumBy' (comparing $ dps . getL bChampStats) bs

main :: IO ()
main = do
    putStrLn "What's up?"
    putStrLn $ show createBestBuild
