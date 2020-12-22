import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Set as S

import Debug.Trace

parse :: String -> ([Int], [Int])
parse = (\[l,r] -> (l,r)) . map (map read . tail . lines) . splitOn "\n\n"

solve1 :: [Int] -> [Int] -> Int
solve1 (x:xs) (y:ys) = case compare x y of
    LT -> solve1 xs (ys ++ [y, x])
    GT -> solve1 (xs ++ [x, y]) ys
solve1 xs [] = sum . zipWith (*) [1..] . reverse $ xs
solve1 [] ys = solve1 ys []

solve2 :: [Int] -> [Int] -> Int
solve2 xs ys = uncurry score . go xs $ ys
    where
        score xs [] = sum . zipWith (*) [1..] . reverse $ xs
        score [] ys = score ys []
        go = go1 S.empty
        go1 _ xs [] = (xs, [])
        go1 _ [] ys = ([], ys)
        go1 s xs ys | (xs, ys) `S.member` s = (xs, [])
                    | otherwise             = go2 (S.insert (xs, ys) s) xs ys
        go2 s (x:xs) (y:ys)
            | null (drop (x-1) xs) || null (drop (y-1) ys) = case compare x y of
                LT -> go1 s xs (ys ++ [y, x])
                GT -> go1 s (xs ++ [x, y]) ys
            | otherwise = case go (take x xs) (take y ys) of
                (_, []) -> go1 s (xs ++ [x, y]) ys
                ([], _) -> go1 s xs (ys ++ [y, x])

main :: IO ()
main = do
    input <- parse <$> readFile "input"
    print . uncurry solve1 $ input
    print . uncurry solve2 $ input
