module Handler.Bravery where

import Import

getBraveryR :: Handler RepHtml
getBraveryR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "Lollerskates ~ Ultimate Bravery!"
        $(widgetFile "bravery")
