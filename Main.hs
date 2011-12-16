module Main where

import Data.List
import System.Console.GetOpt
import System.Environment

import Loller

data Flag = Boots
    deriving (Show)

options :: [OptDescr Flag]
options =
    [ Option "b" ["boots"] (NoArg Boots) "Require boots"
    ]

errHeader :: String
errHeader = "Usage: loller [OPTIONS]"

errMsg :: [String] -> String
errMsg errors = "\n" ++ concat errors ++ usageInfo errHeader options

parseArgv :: [String] -> IO [Flag]
parseArgv argv = case getOpt Permute options argv of
    (flags, params, []) -> return flags
    (_, _, errors) -> fail $ errMsg errors

main = do
    argv <- getArgs
    flags <- parseArgv argv
    return ()
