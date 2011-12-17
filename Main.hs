module Main where

import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.STRef
import System.Console.GetOpt
import System.Environment

import FD
import Loller

data Flag = Boots
          | Unique
    deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option "b" ["boots"] (NoArg Boots) "Require boots"
    , Option "u" ["unique"] (NoArg Unique) "Require unique builds"
    ]

errHeader :: String
errHeader = "Usage: loller [OPTIONS] ATTRIBUTE ITEMS"

errMsg :: [String] -> String
errMsg errors = "\n" ++ concat errors ++ usageInfo errHeader options

parseArgv :: [String] -> IO ([Flag], [String])
parseArgv argv = case getOpt Permute options argv of
    (flags, params, []) -> return (flags, params)
    (_, _, errors) -> fail $ errMsg errors

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Given a padding and a list, pad (or truncate!) the list to a certain
--   length.
pad :: Int -> a -> [a] -> [a]
pad len padding l = take len $ l ++ replicate len padding

lookupAttribute :: Monad m => String -> m (Stats -> Int)
lookupAttribute attr = do
    case Map.lookup (map toLower attr) attributeFilters of
        Just f -> return f
        Nothing -> fail $ "Couldn't match attribute " ++ attr

lookupItem :: Monad m => String -> m [Item]
lookupItem "*" = return [Empty ..]
lookupItem name = do
    case maybeRead name of
        Just item -> return [item]
        Nothing -> fail $ "Couldn't match item name " ++ name

parseArguments :: Monad m => [String] -> m ((Stats -> Int), [[Item]])
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

main = do
    argv <- getArgs
    (flags, params) <- parseArgv argv
    (attribute, sets) <- parseArguments params
    let build = buildForFlags flags sets
    when (null build) $ fail
        $ "No builds match the given constraints: " ++ show (tail params)
    putStrLn $ show $ maxBuild attribute build
    return ()
