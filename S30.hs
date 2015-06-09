module Main where

main = print s30

todigits :: Integer ->[Integer]
todigits n = map (read.(:[])) $ show n

powerDigit n m = let digs = todigits n in sum $ map (^ m) digs

s30 = sum $ filter (\n -> n == powerDigit n 5) [10000..99999]