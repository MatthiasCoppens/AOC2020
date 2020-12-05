import Data.List

pass2binary :: String -> [Bool]
pass2binary = map bit
    where
        bit 'F' = False
        bit 'L' = False
        bit _   = True

binary2int :: [Bool] -> Int
binary2int = foldl (\x y -> 2*x + fromEnum y) 0

solve2 :: [Int] -> Int
solve2 (x1:xs@(x2:_)) | succ x1 == x2 = solve2 xs
                      | otherwise     = succ x1
solve2 _ = error "No seat found!"

main :: IO ()
main = do
    bins <- map pass2binary . lines <$> readFile "input"
    print . binary2int . maximum $ bins
    print . solve2 . sort . map binary2int $ bins
