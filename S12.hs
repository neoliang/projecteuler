module Main where

main = putStrLn "Hello World"

triN :: Integer -> Integer
triN x = x * (1 + x) `div` 2

factors n = [ x | x<-[1..(n `div` 2)] , n `mod` x == 0]

s12 = head . dropWhile ((<200).snd) . map (\x ->(x,length.factors $x)) . map triN $[200..]