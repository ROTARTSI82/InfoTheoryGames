import Text.Regex.Posix
import Data.List

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

matches :: String -> [String] -> [Bool]
matches = map . flip (=~)

count :: [Bool] -> Double
count = fromIntegral . length . filter id

matchesCount :: String -> [String] -> Double
matchesCount = count .: matches

combs :: String -> Char -> [String]
combs [] y = []
combs [x] y = 
    if x == '.' then ["[^" ++ [y] ++ "]", [y]]
    else [[x]]
combs (x:xs) y =
    if x == '.' then [i ++ j | i <- ["[^" ++ [y] ++ "]", [y]], j <- combs xs y]
    else map (x:) (combs xs y)

pos :: [String] -> [String] -> [Double]
pos wordList = map (\word -> matchesCount word wordList)

entropy :: String -> Char -> [String] -> Double
entropy currentWord charGuess wordList = 
    negate . sum $ filter (not . isNaN) (zipWith (*) p (map (logBase 2) p))
    where p = map (/ matchesCount currentWord wordList) (pos wordList (combs currentWord charGuess))

entropyMax :: String -> String -> [String] -> Maybe Char
entropyMax currentWord guessedChars wordList = 
    if maximum h == 0 then Nothing
    else case elemIndex (maximum h) h of
        Just i -> Just ((['a'..'z'] \\ guessedChars) !! i)
        Nothing -> error "Not Found?"
    where h = [entropy currentWord charGuess wordList | charGuess <- ['a'..'z'] \\ guessedChars]

cut :: Char -> [String] -> String -> [String]
cut charGuess wordList currentWord = 
    filter (flip (=~) reg) wordList
    where reg = concat $ map (\c -> if c == '.' then "[^" ++ [charGuess] ++ "]" else [c]) currentWord
    
ask :: String -> String -> [String] -> IO ()
ask currentWord guessedChars wordList = 
    case entropyMax currentWord guessedChars wordList of
        Nothing -> putStrLn ("Solved: " ++ (filter (flip (=~) currentWord) wordList) !! 0)
        Just charGuess -> do
            putStrLn $ (:) charGuess "?"
            getLine >>= flip ask (charGuess:guessedChars) <*> cut charGuess wordList

main :: IO ()
main = do
    word <- getLine
    ask word "" ["cat", "rat", "bat", "att"]
