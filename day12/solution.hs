data Direction = N | E | S | W | F | L | R deriving Read

parse :: String -> [(Direction, Int)]
parse = map (\(c:cs) -> (read [c], read cs)) . lines

solve1, solve2 :: [(Direction, Int)] -> Int

solve1 = go (0, 0) (1, 0)
    where
        go (x, y) _ [] = abs x + abs y
        go (x, y) (dx, dy) (t:rest) = case t of
            (_, 0  ) -> go (x,      y     ) ( dx,  dy) rest
            (N, n  ) -> go (x,      y+n   ) ( dx,  dy) rest
            (E, n  ) -> go (x+n,    y     ) ( dx,  dy) rest
            (S, n  ) -> go (x,      y-n   ) ( dx,  dy) rest
            (W, n  ) -> go (x-n,    y     ) ( dx,  dy) rest
            (F, n  ) -> go (x+n*dx, y+n*dy) ( dx,  dy) rest
            (L, 90 ) -> go (x,      y     ) (-dy,  dx) rest
            (L, 180) -> go (x,      y     ) (-dx, -dy) rest
            (L, 270) -> go (x,      y     ) ( dy, -dx) rest
            (R, 90 ) -> go (x,      y     ) ( dy, -dx) rest
            (R, 180) -> go (x,      y     ) (-dx, -dy) rest
            (R, 270) -> go (x,      y     ) (-dy,  dx) rest

solve2 = go (0, 0) (10, 1)
    where
        go (x, y) _ [] = abs x + abs y
        go (x, y) (x', y') (t:rest) = case t of
            (_, 0  ) -> go (x,      y     ) ( x',   y'  ) rest
            (N, n  ) -> go (x,      y     ) ( x',   y'+n) rest
            (E, n  ) -> go (x,      y     ) ( x'+n, y'  ) rest
            (S, n  ) -> go (x,      y     ) ( x',   y'-n) rest
            (W, n  ) -> go (x,      y     ) ( x'-n, y'  ) rest
            (F, n  ) -> go (x+n*x', y+n*y') ( x',   y'  ) rest
            (L, 90 ) -> go (x,      y     ) (-y',   x'  ) rest
            (L, 180) -> go (x,      y     ) (-x',  -y'  ) rest
            (L, 270) -> go (x,      y     ) ( y',  -x'  ) rest
            (R, 90 ) -> go (x,      y     ) ( y',  -x'  ) rest
            (R, 180) -> go (x,      y     ) (-x',  -y'  ) rest
            (R, 270) -> go (x,      y     ) (-y',   x'  ) rest

main :: IO ()
main = do
    input <- parse <$> readFile "input"
    print . solve1 $ input
    print . solve2 $ input
