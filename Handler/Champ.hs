module Handler.Champ where

import Import
import Yesod.Form

import qualified Data.Text as T

import Lol.Champs
import Lol.Stats

import Widget

data ChampParams = ChampParams { cpChamp :: Champ, cpLevel :: Level }

champChoices :: [(Text, Champ)]
champChoices = let champList = [Ahri ..]
    in zip (map (T.pack . show) champList) champList

levelChoices :: [(Text, Int)]
levelChoices = let levelList = [1 .. 18]
    in zip (map (T.pack . show) levelList) levelList

champForm :: AForm LollerSite LollerSite ChampParams
champForm = ChampParams
    <$> areq (selectField champChoices) "Champion" Nothing
    <*> areq (selectField levelChoices) "Level" (Just 18)

champWidget :: Champ -> Level -> Widget
champWidget c l = [whamlet|
<h2>#{show c} at Level #{l}
^{champStatsWidget $ champStats c l}
|]

getChampR :: Handler RepHtml
getChampR = do
    ((results, form), enctype) <- runFormGet $ renderDivs champForm
    defaultLayout $ do
        setTitle "Lollerskates ~ Champion Statistics"
        (champ, level) <- case results of
            FormSuccess cp -> return $ (cpChamp cp, cpLevel cp)
            _ -> return (Ahri, 18)
        $(widgetFile "champ")
