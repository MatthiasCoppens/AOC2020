parse :: String -> [(Char, Int)]
parse = map (\(c:cs) -> (c, read cs)) . lines

solve1, solve2 :: [(Char, Int)] -> Int

solve1 = go (0, 0) (1, 0)
    where
        go (x, y) _ [] = abs x + abs y
        go (x, y) (dx, dy) ((dir,n):rest) = case dir of
            'N'            -> go (x,      y+n   ) ( dx,  dy) rest
            'E'            -> go (x+n,    y     ) ( dx,  dy) rest
            'S'            -> go (x,      y-n   ) ( dx,  dy) rest
            'W'            -> go (x-n,    y     ) ( dx,  dy) rest
            'F'            -> go (x+n*dx, y+n*dy) ( dx,  dy) rest
            'L' | n == 90  -> go (x,      y     ) (-dy,  dx) rest
                | n == 180 -> go (x,      y     ) (-dx, -dy) rest
                | n == 270 -> go (x,      y     ) ( dy, -dx) rest
            'R' | n == 90  -> go (x,      y     ) ( dy, -dx) rest
                | n == 180 -> go (x,      y     ) (-dx, -dy) rest
                | n == 270 -> go (x,      y     ) (-dy,  dx) rest

solve2 = go (0, 0) (10, 1)
    where
        go (x, y) _ [] = abs x + abs y
        go (x, y) (x', y') ((dir,n):rest) = case dir of
            'N'            -> go (x,      y     ) ( x',   y'+n) rest
            'E'            -> go (x,      y     ) ( x'+n, y'  ) rest
            'S'            -> go (x,      y     ) ( x',   y'-n) rest
            'W'            -> go (x,      y     ) ( x'-n, y'  ) rest
            'F'            -> go (x+n*x', y+n*y') ( x',   y'  ) rest
            'L' | n == 90  -> go (x,      y     ) (-y',   x'  ) rest
                | n == 180 -> go (x,      y     ) (-x',  -y'  ) rest
                | n == 270 -> go (x,      y     ) ( y',  -x'  ) rest
            'R' | n == 90  -> go (x,      y     ) ( y',  -x'  ) rest
                | n == 180 -> go (x,      y     ) (-x',  -y'  ) rest
                | n == 270 -> go (x,      y     ) (-y',   x'  ) rest

main :: IO ()
main = do
    input <- parse <$> readFile "input"
    print . solve1 $ input
    print . solve2 $ input
