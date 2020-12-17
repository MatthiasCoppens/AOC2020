import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Cube = S.Set (Int, Int, Int)
type Cube2 = S.Set (Int, Int, Int, Int)

parse :: String -> Cube
parse = go 0 0 S.empty
    where
        go x y s (c:cs) = case c of
            '#'  -> go (x+1) y (S.insert (x, y, 0) s) cs
            '.'  -> go (x+1) y s cs
            '\n' -> go 0 (y+1) s cs
        go _ _ s [] = s

neighbours :: Cube -> M.Map (Int, Int, Int) Int
neighbours s = M.unionsWith (+)
    [ M.mapKeysMonotonic (\(x, y, z) -> (x+dx, y+dy, z+dz)) m
    | let m = M.fromSet (const 1) s
    , dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dz <- [-1, 0, 1]
    , dx /= 0 || dy /= 0 || dz /= 0
    ]

neighbours2 :: Cube2 -> M.Map (Int, Int, Int, Int) Int
neighbours2 s = M.unionsWith (+)
    [ M.mapKeysMonotonic (\(x, y, z, t) -> (x+dx, y+dy, z+dz, t+dt)) m
    | let m = M.fromSet (const 1) s
    , dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dz <- [-1, 0, 1]
    , dt <- [-1, 0, 1]
    , dx /= 0 || dy /= 0 || dz /= 0 || dt /= 0
    ]

next :: Cube -> Cube
next s =
    let ns = neighbours s
    in  S.union
            (M.keysSet $ M.filter (`elem` [2, 3]) (ns `M.restrictKeys` s))
            (M.keysSet $ M.filter (== 3) (ns `M.withoutKeys` s))

next2 :: Cube2 -> Cube2
next2 s =
    let ns = neighbours2 s
    in  S.union
            (M.keysSet $ M.filter (`elem` [2, 3]) (ns `M.restrictKeys` s))
            (M.keysSet $ M.filter (== 3) (ns `M.withoutKeys` s))

solve1 :: Cube -> Int
solve1 = S.size . (!! 6) . iterate next

solve2 :: Cube2 -> Int
solve2 = S.size . (!! 6) . iterate next2

main :: IO ()
main = do
    input <- parse <$> readFile "input"
    print . solve1 $ input
    print . solve2 $ S.mapMonotonic (\(x, y, z) -> (x, y, z, 0)) input
