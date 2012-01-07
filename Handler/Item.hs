module Handler.Item where

import Import
import Yesod.Form

import qualified Data.Text as T

import Lol.Items

import Widget

data ItemParams = ItemParams { ipItem :: Item }

itemChoices :: [(Text, Item)]
itemChoices = let itemList = [Empty ..]
    in zip (map (T.pack . show) itemList) itemList

itemForm :: AForm LollerSite LollerSite ItemParams
itemForm = ItemParams
    <$> areq (selectField itemChoices) "Item" (Just RubyCrystal)

getItemR :: Handler RepHtml
getItemR = do
    ((results, form), enctype) <- runFormGet $ renderDivs itemForm
    defaultLayout $ do
        setTitle "Lollerskates ~ Item Statistics"
        item <- case results of
            FormSuccess ip -> return $ ipItem ip
            _ -> return RubyCrystal
        $(widgetFile "item")
