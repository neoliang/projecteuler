module Main where

main = print s25

fibs = map fst $ iterate (\(n,m)->(m,m+n)) (1,1)
s25 = snd.head . dropWhile((<1000) . length . show . fst) $ zip fibs [1..]