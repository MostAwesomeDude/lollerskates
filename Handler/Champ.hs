module Handler.Champ where

import Import
import Yesod.Form

import Data.Lens.Common
import qualified Data.Text as T

import Lol.Build
import Lol.Champs
import Lol.Items
import Lol.Stats

import Widget

data ChampParams = ChampParams { cpChamp :: Champ
                               , cpLevel :: Level
                               , cpItem1 :: Item
                               , cpItem2 :: Item
                               , cpItem3 :: Item
                               , cpItem4 :: Item
                               , cpItem5 :: Item
                               , cpItem6 :: Item }

champChoices :: [(Text, Champ)]
champChoices = let champList = [Ahri ..]
    in zip (map (T.pack . show) champList) champList

levelChoices :: [(Text, Int)]
levelChoices = let levelList = [1 .. 18]
    in zip (map (T.pack . show) levelList) levelList

itemChoices :: [(Text, Item)]
itemChoices = let itemList = [Empty ..]
    in zip (map (T.pack . show) itemList) itemList

champForm :: AForm LollerSite LollerSite ChampParams
champForm = ChampParams
    <$> areq (selectField champChoices) "Champion" Nothing
    <*> areq (selectField levelChoices) "Level" (Just 18)
    <*> areq (selectField itemChoices) "Item 1" (Just Empty)
    <*> areq (selectField itemChoices) "Item 2" (Just Empty)
    <*> areq (selectField itemChoices) "Item 3" (Just Empty)
    <*> areq (selectField itemChoices) "Item 4" (Just Empty)
    <*> areq (selectField itemChoices) "Item 5" (Just Empty)
    <*> areq (selectField itemChoices) "Item 6" (Just Empty)

champWidget :: Champ -> Level -> [Item] -> Widget
champWidget c l b =
    let build = makeBuild c l b
        champstats = bChampStats ^$ build
    in [whamlet|
<h2>#{show c} at Level #{l} with items #{show b}
^{champStatsWidget champstats}
|]

repack :: ChampParams -> (Champ, Level, [Item])
repack (ChampParams c l i1 i2 i3 i4 i5 i6) = (c, l, [i1,i2,i3,i4,i5,i6])

getChampR :: Handler RepHtml
getChampR = do
    ((results, form), enctype) <- runFormGet $ renderDivs champForm
    defaultLayout $ do
        setTitle "Lollerskates ~ Champion Statistics"
        (champ, level, items) <- case results of
            FormSuccess cp -> return $ repack cp
            _ -> return (Ahri, 18, [Empty, Empty, Empty, Empty, Empty, Empty])
        $(widgetFile "champ")
