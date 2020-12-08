import qualified Data.Set as Set

data Instr = Acc Int | Jmp Int | Nop Int deriving Show

parse :: String -> [Instr]
parse = map tr . lines
    where
        num = read . dropWhile (=='+')
        tr l = case words l of
            ["acc", n] -> Acc $ num n
            ["jmp", n] -> Jmp $ num n
            ["nop", n] -> Nop $ num n

run1 :: [Instr] -> Int
run1 prog = go 0 Set.empty 0
    where
        l = length prog
        go ip ips acc
            | ip `Set.member` ips = acc
            | ip < 0 || ip >= l   = go (ip `mod` l) ips acc
            | otherwise           = case prog !! ip of
                Acc n -> go (succ ip) (Set.insert ip ips) (acc+n)
                Jmp n -> go (n +  ip) (Set.insert ip ips)  acc
                Nop _ -> go (succ ip) (Set.insert ip ips)  acc

run2 :: [Instr] -> Int
run2 prog = go [(0, Set.empty, False, 0)]
    where
        l = length prog
        go ((ip, ips, ch, acc):rest)
            | ip `Set.member` ips = go rest
            | ip == l && ch       = acc
            | ip < 0 || ip >= l   = go ((ip`mod`l, ips, ch, acc) : rest)
            | otherwise           =
                let ips' = Set.insert ip ips
                in  case prog !! ip of
                        Acc n      -> go ((succ ip, ips', ch, acc+n):rest)
                        Jmp n | ch -> go ((n +  ip, ips', ch, acc  ):rest)
                        Jmp n      -> go ((n +  ip, ips', ch, acc  ):(succ ip, ips', True, acc):rest)
                        Nop n | ch -> go ((succ ip, ips', ch, acc  ):rest)
                        Nop n      -> go ((succ ip, ips', ch, acc  ):(n +  ip, ips', True, acc):rest)

main :: IO ()
main = do
    input <- parse <$> readFile "input"
    print . run1 $ input
    print . run2 $ input
