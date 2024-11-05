{-# LANGUAGE TupleSections #-}
import qualified Data.HashMap.Strict as M
import Data.List (sortOn)
import Data.Ord (Down (..))
import Text.Regex.Posix

mask :: Char -> [String] -> [String]
mask guess wordList = [[if letter == guess then '1' else '0' | letter <- word] | word <- wordList]

count :: [String] -> [Double]
count = map snd . M.toList . M.fromListWith (+) . map (, 1)

entropy :: Char -> [String] -> Double
entropy guess wordList = 
    (0 -) . sum $ map (\p -> p * logBase 2 p) ps
    where 
        counts = count $ mask guess wordList
        ps = map (/sum counts) counts

entropyMax :: [String] -> Maybe Char
entropyMax wordList = 
    case head t of
        (_, 0) -> Nothing
        (letter, _) -> Just letter
    where t = sortOn (Down . snd) [(letter, entropy letter wordList) | letter <- ['a'..'z']]

cut :: Char -> [String] -> String -> [String]
cut charGuess wordList currentWord = 
    filter  (=~ reg) wordList
    where reg = concatMap (\c -> if c == '.' then "[^" ++ [charGuess] ++ "]" else [c]) currentWord
    
ask :: String -> [String] -> IO ()
ask currentWord wordList = do
    -- print $ length wordList
    -- print wordList
    case entropyMax wordList of
        Nothing -> putStrLn ("Solved: " ++ head (filter (=~ currentWord) wordList))
        Just charGuess -> do
            putStrLn $ (:) charGuess "?"
            getLine >>= ask <*> cut charGuess wordList

main :: IO ()
main = do
    word <- getLine
    contents <- readFile "words.txt"
    let wordList = filter ((== length word) . length) $ words contents
    ask word wordList