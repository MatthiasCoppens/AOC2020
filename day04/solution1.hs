import Data.List
import Data.List.Split

valid :: [String] -> Bool
valid password = filter (/= "cid") (sort password) == ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"] && length password <= 8

main :: IO ()
main = print =<< length . filter valid . map concat . splitWhen null . map (map (takeWhile (/= ':')) . words) . lines <$> readFile "input"
