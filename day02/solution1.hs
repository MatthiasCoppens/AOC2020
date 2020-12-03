import Text.ParserCombinators.ReadP

data Policy = Policy Int Int Char deriving Show

parse :: ReadP a -> String -> a
parse p = head . map fst . filter (null . snd) . readP_to_S p

policy :: ReadP Policy
policy =
    let int = readS_to_P reads
    in  Policy <$> int <*> (char '-' *> int) <*> (skipSpaces *> get)

oneline :: ReadP (Policy, String)
oneline = (,) <$> policy <*> (char ':' *> skipSpaces *> many get)

isValidPassword :: String -> Bool
isValidPassword line =
    let (Policy alpha omega c, password) = parse oneline line
        n = length $ take (omega+1) $ filter (== c) password
    in  alpha <= n && n <= omega

main :: IO ()
main = print =<< length . filter isValidPassword . lines <$> readFile "input"
