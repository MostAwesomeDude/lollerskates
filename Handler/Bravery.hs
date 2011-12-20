module Handler.Bravery where

import Import

import Lol.Bravery
import Lol.Items

itemsWidget :: [Item] -> Widget
itemsWidget is = [whamlet|
$forall i <- is
    <li>#{show i}
|]

buildWidget :: [Item] -> Widget
buildWidget is
    | not (null is) && isBoots (head is) = [whamlet|
<ul>
    <li>Boots: #{show $ head is}
    ^{itemsWidget is}
|]
    | not (null is) = [whamlet|<ul>^{itemsWidget is}|]
    | otherwise = [whamlet|<p>Implementation error: buildWidget: empty list|]

braveryWidget :: Bravery -> Widget
braveryWidget b = [whamlet|
<h2>Bravery:
^{buildWidget $ bBuild b}
|]

getBraveryR :: Handler RepHtml
getBraveryR = do
    defaultLayout $ do
        setTitle "Lollerskates ~ Ultimate Bravery!"
        bravery <- return $ braveryWidget =<< liftIO makeBravery
        $(widgetFile "bravery")
