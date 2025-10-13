module Json_Parser where
import System.Exit (die)


test :: String -> String -> IO()
test m t = putStrLn $ "Machine: " ++ m ++ " | tape: " ++ t