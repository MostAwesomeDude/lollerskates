module Handler.Bravery where

import Import
import Yesod.Form

import Lol.Bravery
import Lol.Items

data BraveryParams = BraveryParams { bpRequireBoots :: Bool }

itemsWidget :: [Item] -> Widget
itemsWidget is = let zipped = zip [1 ..] is
    in [whamlet|
$forall (index, i) <- zipped
    <li>Item #{show index}: #{prettyItem i}
|]

buildWidget :: [Item] -> Widget
buildWidget is
    | not (null is) && isBoots (head is) = [whamlet|
<ul>
    <li>Boots: #{prettyItem $ head is}
    ^{itemsWidget $ tail is}
|]
    | not (null is) = [whamlet|<ul>^{itemsWidget is}|]
    | otherwise = [whamlet|<p>Implementation error: buildWidget: empty list|]

braveryWidget :: Bravery -> Widget
braveryWidget b = [whamlet|
<h2>Champion
<p>#{show $ bChamp b}
<h2>Spells
<p>#{show $ bSpells b}
<h2>Build
^{buildWidget $ bBuild b}
<h3>Max your #{show $ bAbility b} first!
|]

braveryForm :: AForm LollerSite LollerSite BraveryParams
braveryForm = BraveryParams
    <$> areq boolField "Require boots?" (Just True)

pickBraveryType :: Monad m => Bool -> m Widget
pickBraveryType t =
    let f = if t then makeBootsBravery else makeBravery
    in return $ braveryWidget =<< liftIO f

getBraveryR :: Handler RepHtml
getBraveryR = do
    ((results, form), enctype) <- runFormGet $ renderDivs braveryForm
    defaultLayout $ do
        setTitle "Lollerskates ~ Ultimate Bravery!"
        bravery <- case results of
            FormSuccess bp -> pickBraveryType $ bpRequireBoots bp
            _ -> pickBraveryType True
        $(widgetFile "bravery")
