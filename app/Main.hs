module Main where
import System.Environment (getArgs)
import Data.Char (toLower)
import System.Exit (die, exitSuccess)
import qualified Json_Parser as JP
import TapeValidator (validateTape)
import TuringMachine (executeMachine)

main :: IO ()
main = do
    args <- getArgs
    let loweredArgs = map (map toLower) args
        wantsHelp = any (\a -> a == "-h" || a == "--help") loweredArgs
        usage = "Usage: cabal run ftTuring -- [-h] jsonfile input"
    if wantsHelp
        then do
            putStrLn usage
            putStrLn $ "\npositional arguments:\n"
                        ++ "    jsonfile\tjson description of the machine\n"
                        ++ "    input\t\tinput of the machine\n\n"
                        ++ "optional arguments:\n"
                        ++ "    -h, --help\tshow this help message and exit"
            exitSuccess
        else case args of   [machine, tape] ->  run machine tape
                            _               ->  die $ "Wrong number of args. " ++ usage


run :: FilePath -> String -> IO()
run machinePath tape = do
    machine <- JP.parseMachineFile machinePath

    case validateTape machine tape of
        Left errorMsg -> die errorMsg
        Right () -> executeMachine machine tape
