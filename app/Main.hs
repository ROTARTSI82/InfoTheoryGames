import qualified Data.HashMap.Strict as M
import Data.List (sortOn)
import Data.List.Split
import Data.Ord (Down (..))
import Text.Regex.Posix

mask :: Char -> [(String, Int)] -> [(String, Int)]
-- mask guess wordList = [([if letter == guess then '1' else '0' | letter <- fst word], 1) | word <- wordList] -- Uniform
mask guess wordList = [([if letter == guess then '1' else '0' | letter <- fst word], snd word) | word <- wordList] -- Frequency

count :: [(String, Int)] -> [Double]
count = map (fromIntegral . snd) . M.toList . M.fromListWith (+)

entropy :: Char -> [(String, Int)] -> Double
entropy guess wordList = 
    (0 -) . sum $ map (\p -> p * logBase 2 p) ps
    where 
        counts = count $ mask guess wordList
        ps = map (/sum counts) counts

entropyMax :: [(String, Int)] -> Maybe Char
entropyMax wordList = 
    case head t of
        (_, 0) -> Nothing
        (letter, _) -> Just letter
    where t = sortOn (Down . snd) [(letter, entropy letter wordList) | letter <- ['a'..'z']]

cut :: Char -> [(String, Int)] -> String -> [(String, Int)]
cut charGuess wordList currentWord = 
    filter (\word -> fst word =~ reg) wordList
    where reg = concatMap (\c -> if c == '.' then "[^" ++ [charGuess] ++ "]" else [c]) currentWord
    
ask :: String -> [(String, Int)] -> IO ()
ask currentWord wordList = do
    -- print $ length wordList
    -- print wordList
    case entropyMax wordList of
        Nothing -> putStrLn $ (++) "Solved: " $ head $ filter (=~ currentWord) $ map fst wordList
        Just charGuess -> do
            putStrLn $ charGuess:"?"
            getLine >>= ask <*> cut charGuess wordList

parse :: String -> (String, Int)
parse word = (head (splitOn "," word), read $ splitOn "," word !! 1)

main :: IO ()
main = do
    contents <- readFile "wordFreq.csv"
    let wordList = map parse $ take 50000 $ words contents
    word <- getLine
    let newList = filter ((== length word) . length . fst) wordList
    ask word newList