import Text.Regex.Posix

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

main :: IO ()
main = do
    print ( entropy "..t" 'c' ["cat", "rat", "bat", "ate"] )
