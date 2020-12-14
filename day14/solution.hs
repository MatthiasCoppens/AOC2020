import Data.Bits
import Data.Char
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP

data Instr = SetMask String | SetMem Int Int deriving Show

mask1 :: String -> Int -> Int
mask1 = go [35,34..]
    where
        go (i:is) ('0':bs) = flip clearBit i . go is bs
        go (i:is) ('1':bs) = flip setBit i . go is bs
        go (_:is) ('X':bs) = go is bs
        go _ [] = id

mask2 :: String -> Int -> [Int]
mask2 s = go [35,34..] s . (:[])
    where
        go (_:is) ('0':bs) = go is bs
        go (i:is) ('1':bs) = map (flip setBit i) . go is bs 
        go (i:is) ('X':bs) = \ns -> [f n i | f <- [setBit, clearBit], n <- go is bs ns]
        go _ [] = id

setMask :: ReadP Instr
setMask = SetMask <$> between (string "mask = ") (char '\n') (many1 (satisfy (/= '\n')))

setMem :: ReadP Instr
setMem = SetMem
    <$> (read <$> (string "mem[" *> many1 (satisfy isDigit)))
    <*> (read <$> (string "] = " *> many1 (satisfy isDigit) <* char '\n'))

code :: ReadP [Instr]
code = many (setMask +++ setMem)

solve1, solve2 :: [Instr] -> Int

solve1 = go id Map.empty
    where
        go f map (SetMem i n : rest) = go f (Map.insert i (f n) map) rest
        go _ map code = case code of
            (SetMask s : rest) -> go (mask1 s) map rest
            _                  -> Map.foldr (+) 0 map

solve2 = go (:[]) Map.empty
    where
        go f map (SetMem i n : rest) = go f (foldr (flip Map.insert n) map (f i)) rest
        go _ map code = case code of
            (SetMask s : rest) -> go (mask2 s) map rest
            _                  -> Map.foldr (+) 0 map

main :: IO ()
main = do
    input <- fst . last . readP_to_S code <$> readFile "input"
    print . solve1 $ input
    print . solve2 $ input
