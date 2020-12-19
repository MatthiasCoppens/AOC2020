import Data.Char (isDigit)
import qualified Data.IntMap.Strict as M
import Text.ParserCombinators.ReadP

data Rule = Rule Int | Match Char | And Rule Rule | Or Rule Rule

parse :: String -> (M.IntMap Rule, [String])
parse = (\(m, s) -> (m, filter (not . null) $ lines s)) . last . readP_to_S rules
    where
        num = read <$> many1 (satisfy isDigit)
        rule = Rule <$> num
        match = Match <$> (char '"' *> get <* char '"')
        and = And <$> rule <*> (char ' ' *> (rule +++ and))
        or = Or <$> (rule +++ and) <*> (string " | " *> rule +++ and)
        rules = M.fromList <$> sepBy ((,)
            <$> num
            <*> (string ": " *> (rule +++ match +++ and +++ or))) (char '\n')

run :: M.IntMap Rule -> [String] -> Int
run m = length . filter (("" `elem`) . go (Rule 0))
    where
        go _ "" = []
        go (Rule i) s = go (m M.! i) s
        go (Match c) (c':cs) | c == c'   = [cs]
                             | otherwise = []
        go (And l r) s = go l s >>= go r
        go (Or l r) s = go l s ++ go r s

main :: IO ()
main = do
    (m, strs) <- parse <$> readFile "input"
    print $ run m strs
    let m' = M.insert 11 (Or
                (And (Rule 42) (Rule 31))
                (And (Rule 42) (And (Rule 11) (Rule 31)))
            ) $ M.insert 8 (Or
                (Rule 42)
                (And (Rule 42) (Rule 8))
            ) m
    print $ run m' strs
