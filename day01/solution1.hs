solve :: [Int] -> Int
solve (n:ns) | 2020 - n `elem` ns = n * (2020-n)
             | otherwise          = solve ns
solve _ = error "No pair with sum 2020 found!"

main :: IO ()
main = print =<< solve . map read . lines <$> readFile "input"
