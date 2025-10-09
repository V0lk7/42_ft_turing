module Main where
import Data.Char (toUpper, toLower)
-- import qualified Data.Map as M  

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

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x

-- tell :: (Show a) => [a] -> String  
-- tell [] = "The list is empty"  
-- tell (x:[]) = "The list has one element: " ++ show x  
-- tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
-- tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

tell :: (Show a) => [a] -> String
tell xs = case xs of    []      -> "The list is empty"
                        (x:[])  -> "The list has one element: " ++ show x
                        (x:y:[])-> "The list has two elements: " ++ show x ++ " and " ++ show y
                        (x:y:_) -> "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- densityTell :: (RealFloat a) => a -> String  
-- densityTell density  
--     | density < 1.2 = "Wow! You're going for a ride in the sky!"  
--     | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"  
--     | otherwise   = "If it's sink or swim, you're going to sink."  

densityTell :: (RealFloat a) => a -> a -> String  
densityTell mass volume  
    | density < air = "Wow! You're going for a ride in the sky!"  
    | density <= water = "Have fun swimming, but watch out for sharks!"  
    | otherwise   = "If it's sink or swim, you're going to sink."  
    where   density = mass / volume  
            (air, water) = (1.2, 1000.0)

max' :: (Ord a) => a -> a -> a  
-- max' a b | a > b = a | otherwise = b
max' a b  
    | a > b     = a  
    | otherwise = b 

myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT

initials :: String -> String -> String  
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

-- using last
antiInitials :: String -> String -> String
antiInitials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where   f = last firstname
            l = last lastname

-- using pattern matching 
antiInitials' :: String -> String -> String
antiInitials' firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where   f = getLast firstname
            l = getLast lastname

antiInitials'' :: String -> String -> String
antiInitials'' firstname lastname =
    let f = getLast firstname
        l = getLast lastname
    in  [f] ++ ". " ++ [l] ++ "."

getLast :: String -> Char
getLast [x] = x
getLast (_:xs) = getLast xs
--

calcDensities :: (RealFloat a) => [(a, a)] -> [a]  
calcDensities xs = [density m v | (m, v) <- xs]  
    where density mass volume = mass / volume

calcDensities' :: (RealFloat a) => [(a, a)] -> [a]  
calcDensities' xs = [density | (m, v) <- xs, let density = m / v]

-- only return if density float in air
calcDensities'' :: (RealFloat a) => [(a, a)] -> [a]  
calcDensities'' xs = [density | (m, v) <- xs, let density = m / v, density < 1.2] 

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."  
                                               xs -> "a longer list."

describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' xs = case xs of    []      -> error "maximum of empty list"
                            [x]     -> x
                            (x:xs)  -> max x (maximum' xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys = case (xs, ys) of   (_, [])         -> []
                                ([], _)         -> []
                                (x:xs', y:ys')  -> (x,y):zip' xs' ys'

-- zip'' :: [a] -> [b] -> [(a,b)]
-- zip'' _ [] = []
-- zip'' [] _ = []
-- zip'' (x:xs) (y:ys) = (x,y):zip'' xs ys

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15

numLongChains' :: Int  
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))
