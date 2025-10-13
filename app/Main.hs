module Main where
import Json_Parser
import System.Environment (getArgs)
import Data.Char (toLower)
import System.Exit (die, exitSuccess)

main :: IO ()
main = do
    args <- getArgs
    let loweredArgs = map (map toLower) args
        wantsHelp = any (\a -> a == "-h" || a == "--help") loweredArgs
        usage = "Usage: cabal run ftTuring -- 'path to machine' 'tape'\nOr: -h / --help to show this message"
    if wantsHelp
        then do
            putStrLn usage
            exitSuccess
        else case args of   [machine, tape] ->  run machine tape
                            _               ->  die $ "Wrong number of args. " ++ usage


run :: String -> String -> IO()
run m t = do
    Json_Parser.test m t -- TODO : Replace with parsing
    -- TODO : storage -> tape parsing -> launch machine