module Main where
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case args of    [machine, tape] -> run machine tape
                    _               -> die "Wrong number of args. Usage: cabal run ftTuring -- 'path to machine' 'tape'"

run :: String -> String -> IO()
run m t = putStrLn $ "Machine: " ++ m ++ " / tape: " ++ t
-- call to JSON parser