{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}


import qualified Data.HashMap.Strict as M
import Data.List (sortOn, partition, transpose)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))
import Text.Regex.Posix ((=~))
import Prelude hiding (Word)
import Data.Hashable (Hashable)


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


condense :: (Hashable a, Num b) => [(a, b)] -> [(a, b)]
condense = M.toList . M.fromListWith (+)

mask :: Letter -> Word -> Pattern
mask letter word = [if character == letter then '1' else '0' | character <- word]

maskMap :: Letter -> Game -> [[(Pattern, Probability)]]
maskMap letter = map concat . transpose . map (\gameState -> map (map ((mask letter ***) $ (snd gameState *) . fromIntegral)) $ fst gameState)

entropy :: Letter -> Game -> Double
entropy letter game =
    let
        counts = map (map snd . condense) (maskMap letter game)
        ps = map (\count -> map (/ sum count) count) counts
    in
        sum $ map ((0 -) . sum . map (\p -> p * logBase 2 p)) ps

entropyMax :: Game -> Maybe Letter
entropyMax game =
    let
        entropies = sortOn (Down . snd) $ map (id &&& flip entropy game) ['a'..'z']
    in case head entropies of
        (_, 0) -> Nothing
        (letter, _) -> Just letter

lieDist :: Turn -> Probability
lieDist = (**) 0.25 . fromIntegral

cutFilter :: [Pattern] -> Game -> Game
cutFilter patterns = map (zipWith (\pattern -> filter (flip (=~) pattern . fst)) patterns *** id)

cutPartitionEmpty :: Letter -> GameNode -> Turn -> Game
cutPartitionEmpty letter gameNode turn =
    let
        nodes = unzip $ map (partition (notElem letter . fst)) $ fst gameNode
    in
        [(fst nodes, snd gameNode - lieDist turn), (snd nodes, lieDist turn)]

cutPartitionNotEmpty :: Letter -> [Pattern] -> GameNode -> Turn -> Game
cutPartitionNotEmpty letter patterns gameNode turn =
    let
        truth = zipWith (\pattern -> filter (flip (=~) pattern . fst)) patterns $ fst gameNode
        lie = map (filter (notElem letter . fst)) $ fst gameNode
    in
        [(truth, snd gameNode - lieDist turn), (lie, lieDist turn)]

simplify :: Game -> Game
simplify game = head game : filter (not . any null . fst) (tail game)

cut :: Letter -> Pattern -> Game -> Turn -> Game
cut letter raw game turn =
    let
        patterns = map (concatMap (\c -> if c == letter then [letter] else "[^" ++ [letter] ++ "]")) (words raw)
    in (if all (`elem` ". ") raw
        then cutPartitionEmpty letter (head game) turn
        else cutPartitionNotEmpty letter patterns (head game) turn)
    ++ cutFilter patterns (tail game)

ask :: Game -> Turn -> IO ()
ask game turn = 
    case entropyMax game of
        Nothing -> putStrLn $ (++) "Solved: " $ unwords $ map fst $ concat $ fst $ last game
        Just letter -> do
            putStrLn $ letter:"?"
            getLine >>= \pattern -> ask (simplify (cut letter pattern game turn)) (turn + 1)

parse :: String -> WordFreq
parse word = (head (splitOn "," word), read $ splitOn "," word !! 1)

main :: IO ()
main = do
    contents <- readFile "wordFreq2.csv"
    let wordList = map parse $ take 50000 $ words contents
    pattern <- getLine
    let newList = map (\word -> filter ((== length word) . length . fst) wordList) (words pattern)
    ask [(newList, 1)] 1