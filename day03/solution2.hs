skip :: Int -> [a] -> [a]
skip n xs@(x:_) = x : (skip n $ drop n xs)
skip _ _        = []

countTrees :: Int -> Int -> [String] -> Int
countTrees x y = length . filter (=='#') . zipWith (flip (!!)) [0,x..] . map cycle . skip y

main :: IO ()
main = do
    ls <- lines <$> readFile "input"
    let t1 = countTrees 1 1 ls
        t2 = countTrees 3 1 ls
        t3 = countTrees 5 1 ls
        t4 = countTrees 7 1 ls
        t5 = countTrees 1 2 ls
    print $ t1*t2*t3*t4*t5
