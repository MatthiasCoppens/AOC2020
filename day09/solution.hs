solve1 :: [Int] -> [Int] -> Int
solve1 (n:ns) (m:ms) | isSum (n:ns) m = solve1 (ns ++ [m]) ms
                     | otherwise      = m
    where
        isSum (n:ns) m | m `elem` map (+n) ns = True
                       | otherwise            = isSum ns m
        isSum [] _ = False

solve2 :: Int -> [Int] -> Int
solve2 s = go []
    where
        go ns (m:ms) | sum ns <  s = go (ns ++ [m]) ms
                     | sum ns == s = maximum ns + minimum ns
        go (_:ns) ms = go ns ms

main :: IO ()
main = do
    ns <- map read . lines <$> readFile "input"
    let n = uncurry solve1 $ splitAt 25 ns
    print n
    print $ solve2 n ns
