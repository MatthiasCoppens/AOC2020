import Data.List
import Data.List.Split
import Debug.Trace

parse :: String -> (Int, [Int], [Int])
parse s =
    let [l1, l2] = lines s
        (ts, ns) = unzip [(t, n) | (t, n) <- zip (splitOn "," l2) [0..], t /= "x"]
    in  (read l1, map read ts, ns)

solve1 :: Int -> [Int] -> Int
solve1 t ts =
    let minMod = minimum $ map ((-t) `mod`) ts
    in  minMod * head [t' | t' <- ts, (-t) `mod` t' == minMod]

main :: IO ()
main = do
    (t, ts, ns) <- parse <$> readFile "input"
    print $ solve1 t ts
    putStrLn "https://www.dcode.fr/chinese-remainder"
