import qualified Data.IntMap.Strict as M

type Circle = (Int, M.IntMap Int)

parse1 :: String -> Circle
parse1 = ((,)
        <$> head
        <*> (M.fromList . (zip <$> id <*> ((++) <$> tail <*> (:[]) . head))))
    . map (read . (:[]))

parse2 :: String -> Circle
parse2 = ((,)
        <$> head
        <*> (M.fromList . (zip <$> id <*> ((++) <$> tail <*> (:[]) . head))))
    . (++ [10 .. 1000000])
    . map (read . (:[]))

move :: Circle -> Circle
move (n, m) =
    let n1 = m M.! n
        n2 = m M.! n1
        n3 = m M.! n2
        next = m M.! n3
        (maxN, _) = M.findMax m
        descending = [n-1, n-2 .. 1] ++ [maxN, maxN-1 ..]
        small = head $ filter (`notElem` [n1, n2, n3]) descending
        m' = M.union (M.fromList [(n, next), (small, n1), (n3, m M.! small)]) m
    in  (next, m')

unparse :: Circle -> String
unparse (_, m) = go (m M.! 1)
    where
        go 1 = ""
        go n = show n ++ go (m M.! n)

main :: IO ()
main = do
    putStrLn $ unparse $ iterate move (parse1 "496138527") !! 100
    let (_, m) = iterate move (parse2  "496138527") !! 10000000
        n1 = m M.! 1
        n2 = m M.! n1
    print $ n1 * n2
