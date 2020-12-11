import           Data.Map.Strict   (Map, (!?))
import qualified Data.Map.Strict as Map

data Seat = Floor | Empty | Occupied deriving (Eq, Show)

coords :: [[(Int, Int)]]
coords = zipWith zip <$> repeat <*> map repeat $ [0,1..]

parse :: String -> Map (Int, Int) Seat
parse = Map.fromList . concat . zipWith zip coords . (map . map) tr . lines
    where
        tr '.' = Floor
        tr 'L' = Empty
        tr '#' = Occupied

next1, next2 :: Map (Int, Int) Seat -> Map (Int, Int) Seat

next1 grid = Map.mapWithKey f grid
    where
        neighbours (x, y) = filter ((== Just Occupied) . (grid !?))
            [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1),
            (x+1, y), (x+1, y+1)]
        f _ Floor = Floor
        f c Empty    | null $          neighbours c = Occupied
        f c Occupied | null $ drop 3 $ neighbours c = Occupied
        f _ _ = Empty

next2 grid = Map.mapWithKey f grid
    where
        n (x, y) (dx, dy) = case grid !? (x+dx, y+dy) of
            Just Floor    -> n (x+dx, y+dy) (dx, dy)
            Just Occupied -> True
            _             -> False
        neighbours c = filter (n c) [(-1,-1), (-1,0), (-1,1), (0,-1),
            (0,1), (1,-1), (1,0), (1,1)]
        f _ Floor = Floor
        f c Empty    | null $          neighbours c = Occupied
        f c Occupied | null $ drop 4 $ neighbours c = Occupied
        f _ _ = Empty

occupied :: Map a Seat -> Int
occupied = Map.size . Map.filter (== Occupied)

run :: Eq a => (a -> a) -> (a -> a)
run f x =
    let y = f x
    in  if x == y then x else run f y

solve1, solve2 :: Map (Int, Int) Seat -> Int
solve1 = occupied . run next1
solve2 = occupied . run next2

main :: IO ()
main = do
    input <- parse <$> readFile "input"
    print . solve1 $ input
    print . solve2 $ input
