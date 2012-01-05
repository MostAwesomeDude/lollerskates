module Handler.Bravery where

import Import
import Yesod.Form

import Data.Lens.Common

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
buildWidget items@(i:is)
    | isBoots i = [whamlet|
<ul>
    <li>Boots: #{prettyItem i}
    ^{itemsWidget is}
|]
    | otherwise = [whamlet|<ul>^{itemsWidget items}|]
buildWidget [] = [whamlet|<p>Implementation error: buildWidget: empty list|]

braveryWidget :: Bravery -> Widget
braveryWidget b = [whamlet|
<h2>Champion
<p>#{show $ getL bChamp b}
<h2>Spells
<p>#{show $ getL bSpells b}
<h2>Build
^{buildWidget $ getL bBuild b}
<h3>Max your #{show $ getL bAbility b} first!
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
