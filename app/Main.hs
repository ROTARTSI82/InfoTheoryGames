import qualified Data.HashMap.Strict as M
import Data.List (sortOn, partition)
import Data.List.Split
import Data.Ord (Down (..))
import Text.Regex.Posix
import qualified Data.Bifunctor as B
import Prelude hiding (Word)


type Letter = Char
type Word = String
type Pattern = String
type Probability = Double
type Entropy = Double
type Turn = Int

type WordFreq = (Word, Int)
type FreqTable = [WordFreq]
type GameNode = (FreqTable, Probability)
type GameTree = [GameNode]


{- 
 - This function iterates through each letter in each word of the tree, doing a crude AND 
 - operation with the letter and a given letter. For example, the word "bat" with the 
 - letter 'a' yields the pattern "010". The function also flattens the game tree into a 
 - singleton, since the specific game node does not matter for its usage. However, the
 - "probability" that word was chosen does matter, so it computes the probability by
 - multilplying the word frequency by the probability corresponding to the game node it is 
 - in.
 -}
mask :: Letter -> GameTree -> [(Pattern, Probability)]
mask guess wordList = 
    concat [[([if letter == guess then '1' else '0' | letter <- fst word], 
    fromIntegral (snd word) * snd list) | word <- fst list] | list <- wordList]

{-
 - This function condenses any matching patterns, summing their "probabilities". For 
 - example, [("010", 1), ("111", 0.5), ("010", 2.1)] turns into [("010", 3.1),
 - ("111", 0.5)]. The patterns do not matter anymore, so it only returns the 
 - "probabilties". 
 -}
count :: [(Pattern, Probability)] -> [Probability]
count = map snd . M.toList . M.fromListWith (+)

{-
 - This function computes the expected amount of information received by guessing a 
 - character. It uses `mask` and `count` to generate a list of "probabilities". It then 
 - sums over plog2p, after normalizing of course.
 -}
entropy :: Letter -> GameTree -> Entropy
entropy charGuess wordList =
    (0 -) . sum $ map (\p -> p * logBase 2 p) ps
    where
        counts = count $ mask charGuess wordList
        ps = map (/sum counts) counts

{-
 - This function figures out what the best letter to guess is. It iterates over each 
 - letter of the alphabet, and uses `entropy` to calculate the expected information to be 
 - gained. It returns the letter that yields the highest entropy, or nothing if the 
 - maximum entropy is 0 (implying the game has been solved).
 -}
entropyMax :: GameTree -> Maybe Letter
entropyMax wordList =
    case head t of
        (_, 0) -> Nothing
        (letter, _) -> Just letter
    where t = sortOn (Down . snd) [(letter, entropy letter wordList) | letter <- ['a'..'z']]

{-
 - This function is just a pdf of which turn the player lies on.
 -}
lieDist :: Int -> Probability
lieDist = (**) 0.25 . fromIntegral

{-
 - This function removes any word in the game tree that does not fulfill the given pattern.
 - It is only ever used on game nodes where the player has already lied.
 -}
cutFilter :: Pattern -> GameTree -> GameTree
cutFilter reg = map $ B.first $ filter $ flip (=~) reg . fst

{-
 - This function is the first of two partitions. It is used in the circumstance that the
 - player has inputted an empty pattern. In this scenario, the current list of words can be
 - divided into two sections: the player is telling the truth (the guessed letter is not in
 - the word), or the player is lying (the guessed letter appears somewhere in the word). The
 - function returns these two possibilities in the form of a game tree.
 -}
cutPartition1 :: Pattern -> GameNode -> Turn -> GameTree
cutPartition1 reg wordList turn =
    [(fst temp, snd wordList - lieDist turn), (snd temp, lieDist turn)]
    where temp = partition (\word -> fst word =~ reg) $ fst wordList

{-
 - This function is the second of two partitions. It is used in the circumstanec that the
 - player has inputted a non-empty pattern. In this scenario, the current list of words can
 - be divided into two sections: the player is telling the truth (the guessed letter appears
 - at the specified location), or the player is lying (the letter does not appear in the 
 - word). The function returns these two possibilities in the form of a game tree.
 -}
cutPartition2 :: Letter -> Word -> GameNode -> Turn -> GameTree
cutPartition2 charGuess reg wordList turn = 
    [(filter (flip (=~) reg . fst) $ fst wordList, snd wordList - lieDist turn),
    (filter (notElem charGuess . fst) $ fst wordList, lieDist turn)]

{-
 - This function is the master game tree filter. Depending on the inputted pattern, it calls
 - either `cutPartition1` or `cutPartition2` on the first game node (the player has not lied).
 - It calls `cutFilter` on the rest of the game nodes (the player has already lied). By doing
 - so, it cuts out as many words as possible from the list of candidates, while keeping track
 - of the current game state.
 -}
cut :: Letter -> Word -> GameTree -> Turn -> GameTree
cut charGuess currentWord wordList turn =
    (if all ('.' ==) currentWord
        then cutPartition1 reg (head wordList) turn
        else cutPartition2 charGuess reg (head wordList) turn)
     ++ cutFilter reg (tail wordList)
    where reg = concatMap (\c -> if c == charGuess then [charGuess] else "[^" ++ [charGuess] ++ "]") currentWord

{-
 - This function is the main game controller. It uses `entropyMax` to determine the best letter
 - to guess at the current moment. It then calls `cut` to incoroporate the new information gained.
 - It recursively calls itself until `entropyMax` returns nothing. At that point, it knows that
 - the game is solved, and it prints out "Solved" + the first word it finds in the game tree.
 -}
ask :: GameTree -> Turn -> IO ()
ask wordList turn = do
    case entropyMax wordList of
        Nothing -> putStrLn $ (++) "Solved: " $ fst $ head $ concatMap fst (M.toList $ M.fromListWith (+) wordList)
        Just charGuess -> do
            putStrLn $ charGuess:"?"
            pat <- getLine
            ask (cut charGuess pat wordList turn) (turn + 1)

{- This function merely parses a line of csv file data into a word/frequency pair. For example,
 - "word, 1000" is returned as ("word", 1000).
 -}
parse :: String -> WordFreq
parse word = (head (splitOn "," word), read $ splitOn "," word !! 1)

{- This function is the main entry point of the program. It reads in the first k most commonly used
 - words, and uses `parse` to turn them into usable data. It then uses the length of the word to
 - filter down the parsed data such that every word has the correct number of letters. It then passes
 - off to `ask` to continue the game until the end.
 -}
main :: IO ()
main = do
    contents <- readFile "wordFreq2.csv"
    let wordList = map parse $ take 50000 $ words contents -- Reads the first 50000 words
    word <- getLine
    let newList = filter ((== length word) . length . fst) wordList -- Filters by length
    ask [(newList, 1)] 1