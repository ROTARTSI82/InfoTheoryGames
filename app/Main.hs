import Text.Regex.Posix
import Data.List

matches :: String -> [String] -> [Bool]
matches = map . flip (=~)

count :: [Bool] -> Double
count = fromIntegral . length . filter id

matchesCount :: String -> [String] -> Double
matchesCount = ((.) . (.)) count matches

combs :: String -> Char -> [String]
combs [] y = []
combs [x] y = 
    if x == '.' then ["[^" ++ [y] ++ "]", [y]]
    else [[x]]
combs (x:xs) y =
    if x == '.' then [i ++ j | i <- ["[^" ++ [y] ++ "]", [y]], j <- combs xs y]
    else map (x:) (combs xs y)

pos :: [String] -> [String] -> [Double]
pos x y = map (\i -> matchesCount i y) x

entropy :: String -> Char -> [String] -> Double
entropy x y z = 
    negate . sum $ filter (not . isNaN) (zipWith (*) p (map (logBase 2) p))
    where p = map (/ matchesCount x z) (pos (combs x y) z)

entropyMax :: String -> String -> [String] -> Maybe Char
entropyMax x y z = 
    if maximum e == 0 then Nothing
    else case elemIndex (maximum e) e of
        Just n -> Just ((['a'..'z'] \\ y) !! n)
        Nothing -> error "Not Found?"
    
    where e = [entropy x i z | i <- ['a'..'z'] \\ y]

ask :: String -> String -> [String] -> IO ()
ask x y z = 
    case entropyMax x y z of
        Nothing -> putStrLn ("Solved: " ++ (filter (\w -> w =~ x) z) !! 0)
        Just guess -> do
            putStrLn ([guess] ++ "?")
            word <- getLine
            ask word ([guess]++y) z

main :: IO ()
main = do
    word <- getLine
    ask word "" ["cat", "rat", "bat", "ate"]
