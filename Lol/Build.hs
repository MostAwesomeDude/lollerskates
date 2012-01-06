module Lol.Build where

import Prelude

import Data.Lens.Template

import Lol.Champs
import Lol.Items
import Lol.Stats

type BuildItems = (Item, Item, Item, Item, Item, Item)

data Build = Build { _bChamp :: Champ
                   , _bLevel :: Level
                   , _bItems :: BuildItems
                   , _bChampStats :: ChampStats }

$( makeLens ''Build )

-- | Pack build items into a tuple for Build usage.
packBuildItems :: [Item] -> BuildItems
packBuildItems ([a,b,c,d,e,f]) = (a,b,c,d,e,f)
packBuildItems _ =
    error "Implementation error: packBuildItems called with /= 6 items"

makeBuild :: Champ -> Level -> [Item] -> Build
makeBuild c l is =
    let stats = makeChampStats c l is
    in Build c l (packBuildItems is) stats
