
module Main where
import Data.List (maximumBy)
main = putStrLn "Hello World"

collatzLength :: Integer -> Integer -> [(Integer,Integer)] -> Integer
collatzLength n maxN ns
  | n == 1 = 1
  | n < maxN = snd (ns !! fromInteger (n-1)) 
  | even n = 1 + collatzLength (n `div` 2) maxN ns
  | otherwise = 1 + collatzLength (3*n + 1) maxN ns

collatz n
  | n == 1 = [1]
  | even n = n : collatz (n `div` 2)
  | otherwise = n : collatz(3*n +1)

collatzSet [] = []
collatzSet (x:xs) = (fst x , (collatzLength (fst x) (fst x) ys)) :  collatzSet xs
