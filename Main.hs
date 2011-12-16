module Main where

import Control.Monad
import Control.Monad.ST
import Data.List
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
errHeader = "Usage: loller [OPTIONS] ATTRIBUTE"

errMsg :: [String] -> String
errMsg errors = "\n" ++ concat errors ++ usageInfo errHeader options

parseArgv :: [String] -> IO ([Flag], [String])
parseArgv argv = case getOpt Permute options argv of
    (flags, params, []) -> return (flags, params)
    (_, _, errors) -> fail $ errMsg errors

lookupAttribute :: Monad m => [String] -> m (Stats -> Int)
lookupAttribute params = do
    when (null params) $ fail "No attributes given!"
    let param = head params
    case param of
        "health" -> return health
        _ -> fail $ "Couldn't match attribute " ++ param

buildForFlags :: [Flag] -> [[Item]]
buildForFlags flags = runFD $ do
    build <- builds
    when (Unique `elem` flags) $ withVariety build
    labelling build

main = do
    argv <- getArgs
    (flags, params) <- parseArgv argv
    attribute <- lookupAttribute params
    let build = buildForFlags flags
    putStrLn $ show $ maxBuild attribute build
    return ()
