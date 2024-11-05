import qualified Data.HashMap.Strict as M
import Data.List (sortOn, partition)
import Data.List.Split
import Data.Ord (Down (..))
import Text.Regex.Posix
import qualified Data.Bifunctor as B

mask :: Char -> [([(String, Int)], Double)] -> [(String, Double)]
mask guess wordList = 
    concat [[([if letter == guess then '1' else '0' | letter <- fst word], 
    fromIntegral (snd word) * snd list) | word <- fst list] | list <- wordList]

count :: [(String, Double)] -> [Double]
count = map snd . M.toList . M.fromListWith (+)

entropy :: Char -> [([(String, Int)], Double)] -> Double
entropy guess wordList =
    (0 -) . sum $ map (\p -> p * logBase 2 p) ps
    where
        counts = count $ mask guess wordList
        ps = map (/sum counts) counts

entropyMax :: [([(String, Int)], Double)] -> Maybe Char
entropyMax wordList =
    case head t of
        (_, 0) -> Nothing
        (letter, _) -> Just letter
    where t = sortOn (Down . snd) [(letter, entropy letter wordList) | letter <- ['a'..'z']]

lieDist :: Int -> Double
lieDist = (**) 0.5 . fromIntegral

cutFilter :: String -> [([(String, Int)], Double)] -> [([(String, Int)], Double)]
cutFilter reg = map (B.first (filter (\word -> fst word =~ reg)))

cutPartition :: String -> ([(String, Int)], Double) -> Int -> [([(String, Int)], Double)]
cutPartition reg wordList turn =
    [(fst temp, snd wordList - lieDist turn), (snd temp, lieDist turn)]
    where
        temp = partition (\word -> fst word =~ reg) $ fst wordList

cut :: Char -> String -> [([(String, Int)], Double)] -> Int -> [([(String, Int)], Double)]
cut charGuess currentWord wordList turn =
    cutPartition reg (head wordList) turn ++ cutFilter reg (tail wordList)
    where reg = concatMap (\c -> if c == charGuess then [charGuess] else "[^" ++ [charGuess] ++ "]") currentWord

ask :: [([(String, Int)], Double)] -> Int -> IO ()
ask wordList turn = do
    case entropyMax wordList of
        Nothing -> putStrLn $ (++) "Solved: " $ fst $ head $ concatMap fst (M.toList $ M.fromListWith (+) wordList)
        Just charGuess -> do
            putStrLn $ charGuess:"?"
            pat <- getLine
            ask (cut charGuess pat wordList turn) (turn + 1)

parse :: String -> (String, Int)
parse word = (head (splitOn "," word), read $ splitOn "," word !! 1)

main :: IO ()
main = do
    contents <- readFile "wordFreq.csv"
    let wordList = map parse $ take 50000 $ words contents
    word <- getLine
    let newList = filter ((== length word) . length . fst) wordList
    ask [(newList, 1)] 1