solve' :: Int -> [Int] -> Maybe Int
solve' s (n:ns) | s - n `elem` ns = Just $ n * (s-n)
                | otherwise       = solve' s ns
solve' _ _ = Nothing

solve :: [Int] -> Int
solve (n:ns) = case solve' (2020-n) ns of
    Just m  -> n * m
    Nothing -> solve ns
solve _ = error "No triple with sum 2020 found!"

main :: IO ()
main = print =<< solve . map read . lines <$> readFile "input"
