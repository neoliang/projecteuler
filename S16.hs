module Main where

main = print s16


d2i :: Char->Integer
d2i c = read  [c]

s16 = sum $ map d2i $ show (2^1000)