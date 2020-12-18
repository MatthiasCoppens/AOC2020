import Data.Char
import Text.ParserCombinators.ReadP


num, l2r, prec :: ReadP Int

num = read <$> many1 (satisfy isDigit)

l2r = foldl (flip ($)) <$> brack <*> (many ((\o r -> (`o` r)) <$> op <*> brack))
    where
        op = ((+) <$ string " + ") +++ ((*) <$ string " * ")
        brack = (between (char '(') (char ')') l2r) +++ num

prec = ((*) <$> add <*> (string " * " *> prec)) +++ add
    where
        brack = (between (char '(') (char ')') prec) +++ num
        add = ((+) <$> brack <*> (string " + " *> add)) +++ brack


solve :: ReadP Int -> String -> Int
solve parser = sum . fst . head .
    readP_to_S ((sepBy parser (char '\n')) <* skipSpaces <* eof)

main :: IO ()
main = do
    input <- readFile "input"
    print . solve l2r  $ input
    print . solve prec $ input
