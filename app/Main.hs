{-# OPTIONS_GHC -Wno-unused-top-binds     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# LANGUAGE TupleSections                #-}

{-# HLINT ignore "Use first"              #-}


import Control.Monad
import Data.Hashable    (Hashable)
import Data.List        (sortOn, transpose)
import Data.List.Split  (splitOn)
import Data.Ord         (Down (..))
import Graphics.UI.Gtk  (Window, Entry)
import Text.Regex.Posix ((=~))
import Prelude          hiding (Word)

import qualified Data.HashMap.Strict as M

import Graphics (enterPhase, gamePhase, setGameDisplay)


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
maskMap letter
    = map join 
    . transpose 
    . map ((map . map . (mask letter ***) . (. fromIntegral) . (*) . snd) <*> fst)

entropy :: Letter -> Game -> Double
entropy letter game =
    let
        counts = map (map snd . condense) (maskMap letter game)
        -- ps     = map (flip (/) . sum >>= map) counts
        ps     = map (map =<< flip (/) . sum) counts
    in
        sum $ map ((0 -) . sum . map ((*) <*> logBase 2)) ps

entropyMax :: Game -> Maybe (Letter, Double)
entropyMax game =
    let
        entropies = sortOn (Down . snd) $ map (id &&& flip entropy game) ['a'..'z']
    in case head entropies of
        (_, 0)        -> Nothing
        (letter, ent) -> Just (letter, ent)

lieDist :: Turn -> Probability
lieDist _ = 0.25

cutFilter :: [Pattern] -> Game -> Game
cutFilter patterns = map (zipWith (filter . (. fst) . flip (=~)) patterns *** id)

combs :: Int -> [Pattern]
combs 1 = ["0", "1"]
combs x = concat [["0" ++ prev, "1" ++ prev] | prev <- combs (x - 1)]

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

cutPartitionNotEmpty :: Letter -> [Pattern] -> GameNode -> Turn -> Game
cutPartitionNotEmpty letter patterns gameNode turn =
    let
        truth = zipWith (filter . (. fst) . flip (=~)) patterns (fst gameNode)
        lie   = map (filter (notElem letter . fst)) $ fst gameNode
    in
        [(truth, snd gameNode * (1 - lieDist turn)), (lie, snd gameNode * lieDist turn)]

simplify :: Game -> Game
simplify game = head game : filter (not . any null . fst) (tail game)

cut :: Letter -> Pattern -> Game -> Turn -> Game
cut letter raw game turn =
    let
        patterns = map (concatMap (\c -> if c == letter then [letter] else "[^" ++ [letter] ++ "]")) (words raw)
    in 
        ( if   all (`elem` ". ") raw 
          then cutPartitionEmpty letter (head game) turn
          else cutPartitionNotEmpty letter patterns (head game) turn
        ) ++ cutFilter patterns (tail game)

mostLikely :: Game -> [Word]
mostLikely game = map (fst . head . sortOn (Down . snd) . condense . concat) $ transpose $ map fst game

ask :: Game -> Turn -> Entry -> IO ()
ask game turn display = do
    -- print game
    setGameDisplay display [(++) "Most Likely: " $ unwords $ mostLikely game]
    case entropyMax game of
        Nothing -> putStrLn $ (++) "Solved: " $ unwords $ map fst $ concat $ fst $ last game
        Just (letter, ent) -> do
            putStrLn $ letter:"? Expected information: " ++ show ent
            -- getLine >>= \pattern -> ask (simplify (cut letter pattern game turn)) (turn + 1) display

parse :: String -> WordFreq
parse word = (head (splitOn "," word), read $ splitOn "," word !! 1)

ready :: Window -> Pattern -> IO ()
ready window pattern = do
    contents <- readFile "wordFreq2.csv"
    let wordList = map parse $ take 50000 $ words contents
    let newList = map (\word -> filter ((== length word) . length . fst) wordList) (words pattern)
    gamePhase window >>= ask [(newList, 1)] 1
    

main :: IO ()
main = do

    enterPhase ready

    -- pattern <- getLine
    -- let newList = map (\word -> filter ((== length word) . length . fst) wordList) (words pattern)
    -- ask [(newList, 1)] 1