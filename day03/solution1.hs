countTrees :: [String] -> Int
countTrees = length . filter (=='#') . zipWith (flip (!!)) [0,3..] . map cycle

main :: IO ()
main = print =<< countTrees . lines <$> readFile "input"
