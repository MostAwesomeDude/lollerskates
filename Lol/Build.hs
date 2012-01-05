module Lol.Build where

import Data.Lens.Template

import Lol.Champs
import Lol.Items

data Build = Build { _bChamp :: Champ
                   , _bLevel :: Level
                   , _bItems :: (Item, Item, Item, Item, Item, Item) }

$( makeLens ''Build )
