module Handler.Constraint where

import Import
import Yesod.Form

import Data.Lens.Common

import Lol.Build
import Lol.Champs
import Lol.Constraints
import Lol.FD
import Lol.Items
import Lol.Stats

import Widget

buildList :: [[Item]]
buildList = runFD $ do
    build <- defaultBuilds
    labelling build

createBestBuild :: Build
createBestBuild = makeBuild Tryndamere 18 $ head buildList

getConstraintR :: Handler RepHtml
getConstraintR = let build = createBestBuild in defaultLayout $ do
    $(widgetFile "constraint")
