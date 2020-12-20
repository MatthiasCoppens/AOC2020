import Data.Char (isDigit)
import Data.List (tails, transpose)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP

parse :: String -> [(Int, [[Bool]])]
parse = fst . head . readP_to_S (sepBy tile (string "\n\n") <* skipSpaces <* eof)
    where
        bit = (True <$ char '#') +++ (False <$ char '.')
        grid = sepBy (many bit) (char '\n')
        tile = (,)
            <$> (string "Tile " *> (read <$> many1 (satisfy isDigit))
                    <* string ":\n")
            <*> grid

getEdges :: (Int, [[a]]) -> (Int, [[a]])
getEdges (n, bss) = (n,
    (++) <$> id <*> map reverse
    $ [head bss, last bss, map head bss, map last bss])

solve1 :: Eq a => [(Int, [[a]])] -> Int
solve1 = product . take 4 . go . map getEdges
    where
        go ((n, edges):rest)
            | null $ drop 2 $ filter (any (`elem` edges)) $ map snd rest
                = n : go (rest ++ [(n, edges)])
            | otherwise
                =     go (rest ++ [(n, edges)])

merge1 :: Eq a => [[a]] -> [[a]] -> Maybe [[a]]
merge1 g1 g2 = listToMaybe
    [ reverse (tail g1') ++ tail g2''
    | g1'  <- [g1, reverse g1]
    , g2'  <- [g2, reverse g2]
    , g2'' <- take 4 $ iterate (transpose . reverse) g2'
    , head g1' == head g2''
    ]

mergeCol :: Eq a => [[[a]]] -> [[[a]]]
mergeCol (grid:grids) = go [] grids
    where
        go gs' (g:gs) = case merge1 grid g of
            Just g' -> mergeCol (g' : (reverse gs' ++ gs))
            Nothing -> go (g:gs') gs
        go gs [] = (reverse gs ++ [grid])

noBorders :: [[a]] -> [[a]]
noBorders = map (init . tail) . init . tail

merge :: Eq a => [[[a]]] -> [[a]]
merge [grid] = noBorders grid
merge grids  = merge $ map transpose $ keepDoing mergeCol grids
    where
        sameLen [] [] = True
        sameLen (_:xs) (_:ys) = sameLen xs ys
        sameLen _ _ = False
        keepDoing f xs =
            let ys = f xs
            in  (if sameLen xs ys then id else keepDoing f) ys

monstersHoriz :: [[Bool]] -> Int
monstersHoriz ([]:_) = 0
monstersHoriz [] = 0
monstersHoriz grid@((_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:True:_):(True:_:_:_:_:True:True:_:_:_:_:True:True:_:_:_:_:True:True:True:_):(_:True:_:_:True:_:_:True:_:_:True:_:_:True:_:_:True:_):_) = succ $ monstersHoriz $ map tail grid
monstersHoriz grid = monstersHoriz $ map tail grid

monstersVertic :: [[Bool]] -> Int
monstersVertic = sum . map monstersHoriz . tails

monstersOrient :: [[Bool]] -> Int
monstersOrient grid = maximum
    [ monstersVertic g'
    | g  <- [grid, reverse grid]
    , g' <- take 4 $ iterate (transpose . reverse) g
    ]

sea :: [[Bool]] -> Int
sea grid = (sum $ map fromEnum $ concat grid) - 15 * monstersOrient grid

solve2 :: [(a, [[Bool]])] -> Int
solve2 = sea . merge . map snd

main :: IO ()
main = do
    input <- parse <$> readFile "input"
    print . solve1 $ input
    print . solve2 $ input
