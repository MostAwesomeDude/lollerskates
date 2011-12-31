module Handler.Champ where

import Import
import Yesod.Form

import qualified Data.Text as T

import Lol.Champs
import Lol.Stats

import Widget

data ChampParams = ChampParams { cpChamp :: Champ }

champList :: [(Text, Champ)]
champList = let champList = [Ahri ..]
    in zip (map (T.pack . show) champList) champList

champForm :: AForm LollerSite LollerSite ChampParams
champForm = ChampParams
    <$> areq (selectField champList) "Champion" Nothing

champWidget :: Champ -> Widget
champWidget c = [whamlet|
<h2>#{show c} at Level 18
^{champStatsWidget $ champStats c 18}
|]

getChampR :: Handler RepHtml
getChampR = do
    ((results, form), enctype) <- runFormGet $ renderDivs champForm
    defaultLayout $ do
        setTitle "Lollerskates ~ Champion Statistics"
        champ <- case results of
            FormSuccess cp -> return $ cpChamp cp
            _ -> return Ahri
        $(widgetFile "champ")
