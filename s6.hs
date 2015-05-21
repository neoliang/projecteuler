module Main where

main = print (s6 100)

s6 n  = sum[1..n] ^ 2 - (sum $ map (^2) [1..n])