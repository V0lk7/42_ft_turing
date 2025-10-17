module TuringMachine (executeMachine) where

import Json_Parser
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

-- Tape :       (left, current, right)
data Tape = Tape [Char] Char [Char] deriving (Show, Eq)

-- Move head and write
move :: Tape -> Char -> Action -> Char -> Tape
move (Tape l _ (r:rs))  write RIGHT _       = Tape (write:l) r rs
move (Tape l _ [])      write RIGHT blank   = Tape (write:l) blank []
move (Tape (x:xs) _ r)  write LEFT _        = Tape xs x (write:r)
move (Tape [] _ r)      write LEFT blank    = Tape [] blank (write:r)

-- init tape with blanks at the ends
initTape :: String -> Char -> Tape
initTape str blank = Tape [] blank (str ++ [blank])

-- skip starting Blanks
skipBlanks :: Tape -> Char -> Tape
skipBlanks tape blank
    | currentSymbol tape /= blank = tape
    | otherwise =
        let tape' = move tape blank RIGHT blank
        in  if tape' == tape
            then tape
            else skipBlanks tape' blank

-- read current
currentSymbol :: Tape -> Char
currentSymbol (Tape _ c _) = c

-- display tape in terminal with current between < >
showTape :: Tape -> String
showTape (Tape l c r) = reverse l ++ ['<'] ++ [c] ++ ['>'] ++ r

-- display transitions
printTransitions :: Machine -> IO ()
printTransitions m = do
    let trs = M.toList (mTransitions m)
    mapM_ (\(st, ts) -> mapM_ (\tr ->
        putStrLn $ "(" ++ T.unpack st ++ ", " ++ T.unpack (trRead tr) ++ ") -> ("
                    ++ T.unpack (trTo tr) ++ ", " ++ T.unpack (trWrite tr) ++ ", "
                    ++ show (trAct tr) ++ ")") ts) trs

-- display banner
printBanner :: T.Text -> IO ()
printBanner name = do
    let n = T.length name
        line = replicate (fromIntegral n + 4) '*'
        middle = "* " ++ T.unpack name ++ " *"
    putStrLn ""
    putStrLn line
    putStrLn middle
    putStrLn line

-- execute machine on tape
executeMachine :: Machine -> String -> IO ()
executeMachine m input = do
    let blankChar = T.head (mBlank m)
        tape0 = initTape input blankChar
        tapeStart = skipBlanks tape0 blankChar

    -- Display machine infos
    printBanner (mName m)
    putStrLn $ "Alphabet: [ " ++ unwords (map T.unpack $ mAlphabet m) ++ " ]"
    putStrLn $ "States: [ " ++ unwords (map T.unpack $ mStates m) ++ " ]"
    putStrLn $ "Initial: " ++ T.unpack(mInitial m)
    putStrLn $ "Finals: [ " ++ unwords (map T.unpack $ mFinals m) ++ " ]"
    printTransitions m
    putStrLn $ replicate 80 '*'

    -- run
    runMachine m tapeStart

-- machine loop
runMachine :: Machine -> Tape -> IO ()
runMachine machine tape = do

    if mCurrent machine `elem` mFinals machine
        then putStrLn $ "[" ++ showTape tape ++ "]\nMachine halted in final state: " ++ T.unpack (mCurrent machine)
        else do
            let symbol = currentSymbol tape
                transitions = fromMaybe [] (M.lookup (mCurrent machine) (mTransitions machine))
                mTr = findTransition symbol transitions
            case mTr of
                Nothing -> putStrLn $ "No valid transition for state " ++ T.unpack (mCurrent machine)
                                        ++ " reading symbol '" ++ [symbol] ++ "'. Machine halts."
                Just tr -> do
                    let tape' = move tape (T.head (trWrite tr)) (trAct tr) (T.head (mBlank machine))
                        m' = machine { mCurrent = trTo tr }
                    putStrLn $ "[" ++ showTape tape ++ "] (" ++ T.unpack (mCurrent machine) ++ ", "
                                ++ [currentSymbol tape] ++ ") -> (" ++ T.unpack (trTo tr) ++ ", "
                                ++ T.unpack (trWrite tr) ++ ", " ++ show (trAct tr) ++ ")"
                    runMachine m' tape'

-- find transition for read symbol
findTransition :: Char -> [Transition] -> Maybe Transition
findTransition sym = foldr (\tr acc ->  if T.head (trRead tr) == sym
                                        then Just tr
                                        else acc)
                                        Nothing
