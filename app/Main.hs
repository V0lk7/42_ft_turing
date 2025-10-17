module Main where
import System.Environment (getArgs)
import Data.Char (toLower)
import System.Exit (die, exitSuccess)
import qualified Json_Parser as JP

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


run :: FilePath -> String -> IO()
run machinePath _tape = do
    _machine <- JP.parseMachineFile machinePath
    return ()
--    putStrLn "Machine :"
--    print machine

    -- TODO : tape parsing -> launch machine
