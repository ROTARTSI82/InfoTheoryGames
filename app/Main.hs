{-# OPTIONS_GHC -Wno-unused-top-binds     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# LANGUAGE TupleSections                #-}

{-# HLINT ignore "Use first"              #-}


import Data.List (sortOn, transpose)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))
import Prelude hiding (Word)
import Text.Regex.Posix ((=~))
import Data.Hashable (Hashable)

import qualified Data.HashMap.Strict as M


type Turn = Int
type Frequency = Int
type Probability = Double
type Pattern = String
type Letter = Char
type Word = [Letter]
type WordFreq = (Word, Frequency)
type WordNode = [WordFreq]
type PhraseNode = [WordNode]
type GameNode = (PhraseNode, Probability)
type Game = [GameNode]


(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(***) f g (x, y) = (f x, g y)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x)


-- This function takes a list of tuples and combines them based on the first entry
-- and adding the second. For example, [('a', 1), ('b', 2), ('a', 3)] becomes [(a, 4), (b, 2)].
condense :: (Hashable a, Num b) => [(a, b)] -> [(a, b)]
condense = M.toList . M.fromListWith (+)

-- This function gives a 'binary' string that shows where a letter appears in a word. 
-- For example, 'a' "bat" becomes "010".
mask :: Letter -> Word -> Pattern
mask letter word = [if character == letter then '1' else '0' | character <- word]

-- This function takes a Game and maps `mask` over every word.
maskMap :: Letter -> Game -> [[(Pattern, Probability)]]
maskMap letter
    = map concat
    . transpose
    . map
        ( \gameState ->
            map (map ((mask letter ***) $ (snd gameState *) . fromIntegral)) $ fst gameState
        )

-- This function finds the expected information of guessing a letter given a Game.
entropy :: Letter -> Game -> Double
entropy letter game =
    let
        counts = map (map snd . condense) (maskMap letter game)
        ps     = map (\count -> map (/ sum count) count) counts
    in
        sum $ map ((0 -) . sum . map (\p -> p * logBase 2 p)) ps

-- This finds the letter that gives the most information given a Game. It returns
-- both the letter and its corresponding entropy, but if the entropy is 0, it
-- returns nothing.
entropyMax :: Game -> Maybe (Letter, Double)
entropyMax game =
    let
        entropies = sortOn (Down . snd) $ map (id &&& flip entropy game) ['a'..'z']
    in case head entropies of
        (_, 0)        -> Nothing
        (letter, ent) -> Just (letter, ent)

-- This functions gives the probability that the user lies on a given turn.
lieDist :: Turn -> Probability
lieDist _ = 0.25

-- This filters a Game such that any words that do not follow the given pattern
-- are removed.
cutFilter :: [Pattern] -> Game -> Game
cutFilter patterns = map (zipWith (\pattern -> filter (flip (=~) pattern . fst)) patterns *** id)

-- This function finds every combination of '0's and '1's of a given length. 
combs :: Int -> [Pattern]
combs 1 = ["0", "1"]
combs x = concat [["0" ++ prev, "1" ++ prev] | prev <- combs (x - 1)]

-- This function is used to filter the game if the user claims a letter is not in the phrase.
cutPartitionEmpty :: Letter -> GameNode -> Turn -> Game
cutPartitionEmpty letter gameNode turn =
    let
        combinations = tail $ combs $ length $ fst gameNode
        p            = (snd gameNode * lieDist turn) / fromIntegral (length combinations)
    in
        ( map (filter (notElem letter . fst)) $ fst gameNode
        , snd gameNode * (1 - lieDist turn)
        ) : map
            ((, p) . flip (zipWith (\cond -> filter ((if cond == '0' then notElem else elem) letter . fst))) (fst gameNode))
            combinations

-- This function is used to filter the game if the user claims a letter is in the phrase.
cutPartitionNotEmpty :: Letter -> [Pattern] -> GameNode -> Turn -> Game
cutPartitionNotEmpty letter patterns gameNode turn =
    let
        truth = zipWith (\pattern -> filter (flip (=~) pattern . fst)) patterns $ fst gameNode
        lie   = map (filter (notElem letter . fst)) $ fst gameNode
    in
        [(truth, snd gameNode * (1 - lieDist turn)), (lie, snd gameNode * lieDist turn)]

-- This function removes any invalid GameNodes. It does this by finding nodes where the
-- list of possible words is null.
simplify :: Game -> Game
simplify game = head game : filter (not . any null . fst) (tail game)

-- This function is the master filtering function that calls the other `cut` functions 
-- as necessary.
cut :: Letter -> Pattern -> Game -> Turn -> Game
cut letter raw game turn =
    let
        patterns = map (concatMap (\c -> if c == letter then [letter] else "[^" ++ [letter] ++ "]")) (words raw)
    in
        ( if   all (`elem` ". ") raw
          then cutPartitionEmpty letter (head game) turn
          else cutPartitionNotEmpty letter patterns (head game) turn
        ) ++ cutFilter patterns (tail game)

-- This function finds the most likely phrase in the Game.
mostLikely :: Game -> [Word]
mostLikely game = map (fst . head . sortOn (Down . snd) . condense . concat) $ transpose $ map fst game

-- This function finds the second most likely phrase in the Game.
secondLikely :: Game -> [Word]
secondLikely game =
    let
        sec :: [a] -> a
        sec [x] = x
        sec (_:xs) = head xs
    in
        map (fst . sec . sortOn (Down . snd) . condense . concat) $ transpose $ map fst game

-- This function runs the game, repeatedly calling itself to continue playing.
ask :: Game -> Turn -> IO ()
ask game turn = do
    -- print game
    putStrLn $ (++) "Most Likely: " $ unwords $ mostLikely game
    putStrLn $ (++) "Second Most Likely: " $ unwords $ secondLikely game
    case entropyMax game of
        Nothing -> end $ (++) (show turn ++ ". ") $ (++) "Is the phrase (y/n): " $ unwords $ map fst $ concat $ fst $ last game
        Just (letter, ent) -> do
            putStrLn $ show turn ++ ". " ++ letter:"? Expected information: " ++ show ent
            getLine >>= \pattern -> simplify (cut letter pattern game turn) `ask` (turn + 1)

-- This function handles the end of the game, when the program has determined the answer.
end :: Word -> IO ()
end guess = do
    putStrLn guess
    response <- getLine
    if   response == "y"
    then putStrLn "OK"
    else putStrLn "OK you messed up"

-- This function parses the string of characters from a csv file into the necessary format.
parse :: String -> WordFreq
parse word = (head (splitOn "," word), read $ splitOn "," word !! 1)

-- This function is the entry point and sets everything up.
main :: IO ()
main = do
    contents <- readFile "wordFreq2.csv"
    let wordList = map parse $ take 200000 $ words contents

    putStrLn "1. Length of phrase (including spaces)?"
    phraseLength <- getLine
    putStrLn "2. Location of spaces (separated by whitespace and indexed from 1)?"
    spaces <- getLine
    let newPattern = [if i `elem` map read (words spaces) then ' ' else '.'| i <- [1..read phraseLength :: Int]]

    let newList = map (\word -> filter ((== length word) . length . fst) wordList) (words newPattern)
    ask [(newList, 1)] 3