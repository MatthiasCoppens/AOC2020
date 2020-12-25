solve :: Int -> Int -> Int
solve n1 n2 =
    let (l, n) = find [n1, n2] $ keys 7
    in  keys n !! l
    where
        keys n = iterate ((`mod` 20201227) . (* n)) 1 
        find ms = go
            where
                go (n:ns) | n `elem` ms = (0, head (filter (/= n) ms))
                          | otherwise   = let (l, n') = go ns in (succ l, n')

main :: IO ()
main = do
    (n1:n2:_) <- map read . lines <$> readFile "input"
    print $ solve n1 n2
