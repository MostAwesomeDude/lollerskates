{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withLollerSite
    , withDevelAppPort
    ) where

import Import
import Settings.StaticFiles (staticSite)
import Yesod.Default.Config
import Yesod.Default.Main (defaultDevelApp, defaultRunner)
import Yesod.Default.Handlers (getFaviconR, getRobotsR)
import Yesod.Logger (Logger)
import Network.Wai (Application)
import Data.Dynamic (Dynamic, toDyn)

-- Import all relevant handler modules here.
import Handler.Bravery
import Handler.Champ
import Handler.Root

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "LollerSite" resourcesLollerSite

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withLollerSite :: AppConfig DefaultEnv () -> Logger -> (Application -> IO ()) -> IO ()
withLollerSite conf logger f = do
    s <- staticSite
    let h = LollerSite conf logger s
    defaultRunner f h

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withLollerSite
