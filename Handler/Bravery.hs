module Handler.Bravery where

import Import

import Lol.Bravery

braveryWidget :: Bravery -> Widget
braveryWidget b = [whamlet|<p>#{show b}|]

getBraveryR :: Handler RepHtml
getBraveryR = do
    defaultLayout $ do
        setTitle "Lollerskates ~ Ultimate Bravery!"
        bravery <- return $ braveryWidget =<< liftIO makeBravery
        $(widgetFile "bravery")
