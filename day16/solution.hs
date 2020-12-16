import Data.Char
import Data.List
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

data Field = Field
    { name   :: String
    , ranges :: [(Int, Int)]
    } deriving Show
type Ticket = [Int]

parser :: ReadP ([Field], Ticket, [Ticket])
parser = (,,)
    <$> sepBy field (char '\n')
    <*> (string "\n\nyour ticket:\n" *> ticket)
    <*> (string "\n\nnearby tickets:\n" *> sepBy ticket (char '\n'))
    <*  skipSpaces
    where
        number = read <$> many1 (satisfy isDigit)
        field = Field
            <$> many (satisfy (/= ':')) 
            <*> (string ": " *> sepBy1 ((,)
                <$> number
                <*> (char '-' *> number)
            ) (string " or "))
        ticket = sepBy1 number (char ',')

condense :: [(Int, Int)] -> [(Int, Int)]
condense ((a,b):(c,d):rest)
    | b < c     = (a, b) : condense ((c, d) : rest)
    | otherwise = condense ((a, max b d) : rest)
condense l = l

solve1 :: [Field] -> [Ticket] -> Int
solve1 fs ts =
    sum $ filter (test $ condense $ sort $ concatMap ranges fs) $ concat ts
    where
        test ((a,b):rest) x | x < a     = True
                            | x <= b    = False
                            | otherwise = test rest x
        test _ x = True

solve2 fs t ts =
    let rs = condense $ sort $ concatMap ranges fs
        ts' = filter (test rs) ts
    in  end t $ run $ possibilities fs $ map sort $ transpose (t:ts')
    where
        test' ((a,b):rest) (x:xs) | x < a     = False
                                  | x <= b    = test' ((a,b):rest) xs
                                  | otherwise = test' rest (x:xs)
        test' _ [] = True
        test' [] _ = False
        test rs = test' $ condense $ sort rs
        possibilities fs ts =
            [S.fromList [name f | f <- fs, test (ranges f) t] | t <- ts]
        run' ps [] = run $ reverse ps
        run' ps' (p:ps) | S.size p == 1 = run' (p:(map (S.\\p) ps')) (map (S.\\p) ps)
                        | otherwise     = run' (p:ps') ps
        run ps | all ((== 1) . S.size) ps = map (S.elemAt 0) ps
               | otherwise                = run' [] ps
        end (n:ns) (s:ss) = case take 10 s of
            "departure " -> n * end ns ss
            _            ->     end ns ss
        end [] [] = 1

main :: IO ()
main = do
    (fs, t, ts) <- fst . last . readP_to_S parser <$> readFile "input"
    print $ solve1 fs ts
    print $ solve2 fs t ts
