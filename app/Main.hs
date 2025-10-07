module Main where
import Data.Char (toUpper, toLower)

toUpperString :: String -> String
toUpperString = map toUpper

toLowerString :: String -> String
toLowerString = map toLower

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- factorial :: Integer -> Integer
-- factorial n = product [1..n]

main :: IO ()
main = do
    putStrLn "Tape une ligne :"
    line <- getLine
    putStrLn (toUpperString line)

doubleMe x = x * 2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
conanO'Brien = "It's a-me, Conan O'Brien!"
