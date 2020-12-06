import           Data.List.Split
import           Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> [[Set Char]]
parse = map (map Set.fromList . lines) . splitOn "\n\n"

main :: IO ()
main = do
    input <- parse <$> readFile "input"
    print . sum . map (Set.size . Set.unions             ) $ input
    print . sum . map (Set.size . foldl1 Set.intersection) $ input
