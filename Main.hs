{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import System.Console.CmdArgs

import FD
import Items
import Loller

data Flag = Boots
          | Unique
    deriving (Eq, Show)

data ModeParameters = BuildMode { bmAttribute :: String
                                , bmParameters :: [String] }
                    | ItemMode { imAttribute :: String }
    deriving (Data, Show, Typeable)

arguments :: Mode (CmdArgs ModeParameters)
arguments = cmdArgsMode $
    modes [ BuildMode { bmAttribute = def &= argPos 0 &= typ "ATTRIBUTE"
                      , bmParameters = def &= args &= typ "ITEMS" }
                      &= name "build"
          , ItemMode { imAttribute = def &= argPos 0 &= typ "ATTRIBUTE" }
                     &= name "item" ]
    &= summary "Lollerskates"

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Given a padding and a list, pad (or truncate!) the list to a certain
--   length.
pad :: Int -> a -> [a] -> [a]
pad len padding l = take len $ l ++ replicate len padding

lookupAttribute :: Monad m => String -> m Comparator
lookupAttribute attr = case Map.lookup (map toLower attr) attributeFilters of
    Just f -> return f
    Nothing -> fail $ "Couldn't match attribute " ++ attr

lookupItem :: Monad m => String -> m [Item]
lookupItem "*" = return [Empty ..]
lookupItem name = case maybeRead name of
    Just item -> return [item]
    Nothing -> fail $ "Couldn't match item name " ++ name

parseArguments :: Monad m => [String] -> m (Comparator, [[Item]])
parseArguments args = do
    when (null args) $ fail "No arguments given!"
    attribute <- lookupAttribute $ head args
    items <- mapM lookupItem $ pad 6 "*" (tail args)
    return (attribute, items)

buildForFlags :: [Flag] -> [[Item]] -> [Build]
buildForFlags flags items = runFD $ do
    build <- builds items
    when (Unique `elem` flags) $ withVariety build
    labelling build

main :: IO ()
main = do
    args <- cmdArgsRun arguments
    doMode args

doMode :: ModeParameters -> IO ()
doMode (BuildMode attr params) = do
    attribute <- lookupAttribute attr
    sets <- mapM lookupItem $ pad 6 "*" params
    let build = buildForFlags [] sets
    when (null build) $ fail
        $ "No builds match the given constraints: " ++ show (tail params)
    print $ maxBuild attribute build
doMode (ItemMode attr) = do
    attribute <- lookupAttribute attr
    print $ bestItem attribute [Empty ..]
doMode _ = fail $ "Unknown mode. You probably shouldn't be able to reach this."
