import Data.List
import Data.List.Split

parse :: String -> (Int, [Int], [Int])
parse s =
    let [l1, l2] = lines s
        (ts, ns) = unzip [(t, n) | (t, n) <- zip (splitOn "," l2) [0..], t /= "x"]
    in  (read l1, map read ts, ns)

solve1 :: Int -> [Int] -> Int
solve1 t ts =
    let minMod = minimum $ map ((-t) `mod`) ts
    in  minMod * head [t' | t' <- ts, (-t) `mod` t' == minMod]

solve2 :: [Int] -> [Int] -> Int
solve2 ts ns = uncurry go $ unzip $ sort $ zip ts ns
    where
        go [t] [n] = (-n) `mod` t
        go (t:ts) (n:ns) =
            let x0 = go ts ns
            in  head $ filter ((== 0) . (`mod` t) . (+ n)) [x0, x0 + product ts ..]

main :: IO ()
main = do
    (t, ts, ns) <- parse <$> readFile "input"
    print $ solve1 t ts
    print $ solve2 ts ns
