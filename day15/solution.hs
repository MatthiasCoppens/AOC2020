import Data.Maybe
import qualified Data.Map.Strict as M

input :: [Int]
input = [13,0,10,12,1,5,8]

solve :: Int -> [Int] -> Int
solve l = go1 1 M.empty
    where
        go1 turn table (n:ns) = case ns of
            [] -> go2 turn table n
            _  -> go1 (succ turn) (M.insert n turn table) ns
        go2 turn _ n | turn == l = n
        go2 turn table n = go2 (succ turn) (M.insert n turn table) $
            fromMaybe 0 $ (turn -) <$> table M.!? n

main :: IO ()
main = do
    print . solve 2020 $ input
    print . solve 30000000 $ input
