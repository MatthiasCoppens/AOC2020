import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Grid = S.Set (Int, Int)

parse :: String -> Grid
parse = foldr flip S.empty . map coord . lines
    where
        flip x xs
            | x `S.member` xs = S.delete x xs
            | otherwise       = S.insert x xs
        coord ""         = (0, 0)
        coord ('e':cs)   = let (x, y) = coord cs in (x+1, y)
        coord ('w':cs)   = let (x, y) = coord cs in (x-1, y)
        coord (c1:c2:cs) = let (x, y) = coord cs in case [c1, c2] of
            "se" -> (x+1, y-1)
            "sw" -> (x,   y-1)
            "ne" -> (x,   y+1)
            "nw" -> (x-1, y+1)

move :: Grid -> Grid
move s =
    let ns  = neighbours s
        ns1 = ns `M.restrictKeys` s
        ns2 = ns `M.withoutKeys`  s
    in  M.keysSet $ M.union (M.filter (<= 2) ns1) (M.filter (== 2) ns2)
    where
        neighbours s = M.unionsWith (+)
            [ M.fromSet (const 1) $ S.mapMonotonic (\(x, y) -> (x+dx, y+dy)) s
            | (dx, dy) <- [(1, 0), (-1, 0), (1, -1), (0, -1), (0, 1), (-1, 1)]
            ]

main :: IO ()
main = do
    grids <- iterate move . parse <$> readFile "input"
    print . S.size $ grids !! 0
    print . S.size $ grids !! 100
