import Control.Monad
import Data.List
import Data.List.Split
import Text.ParserCombinators.ReadP

data Field = BYR | IYR | EYR | HGT | HCL | ECL | PID | CID
    deriving (Eq, Ord, Show)
data Password = Valid

digit :: ReadP Char
digit = choice $ char <$> "0123456789"

byr = BYR <$ do
    string "byr:"
    n <- read <$> count 4 digit
    guard $ 1920 <= n && n <= 2002
iyr = IYR <$ do
    string "iyr:"
    n <- read <$> count 4 digit
    guard $ 2010 <= n && n <= 2020
eyr = EYR <$ do
    string "eyr:"
    n <- read <$> count 4 digit
    guard $ 2020 <= n && n <= 2030
hgt = HGT <$ do
    string "hgt:"
    n <- read <$> many1 digit
    unit <- string "cm" +++ string "in"
    guard $ if unit == "cm"
        then 150 <= n && n <= 193
        else  59 <= n && n <=  76
hcl = HCL <$ do
    string "hcl:#"
    count 6 $ choice $ char <$> "0123456789abcdef"
ecl = ECL <$ do
    string "ecl:"
    choice $ string <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
pid = PID <$ do
    string "pid:"
    count 9 digit
cid = CID <$ do
    string "cid:"
    many1 $ satisfy (/= ' ')

password :: ReadP Password
password = Valid <$ do
    fields <- sepBy1 (choice [byr, iyr, eyr, hgt, hcl, ecl, pid, cid]) (char ' ')
    guard $ length fields <= 8 && take 7 (sort fields) == [BYR, IYR, EYR, HGT, HCL, ECL, PID]

test :: String -> Bool
test = not . null . filter (null . snd) . readP_to_S password

main :: IO ()
main = print =<< length . filter test . map (intercalate " ") . splitWhen null . lines <$> readFile "input"
