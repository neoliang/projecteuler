
module Main where
import Data.List (maximumBy)
main = putStrLn "Hello World"

collatzLength n
  | n == 1 = 1
  | even n = 1 + collatzLength (n `div` 2)
  | otherwise = 1 + collatzLength (3*n + 1)
collatz n
  | n == 1 = [1]
  | even n = n : collatz (n `div` 2)
  | otherwise = n : collatz(3*n +1)
  
s14 = maximumBy(\ x y -> compare (snd x) (snd y)) . zip [1..1000000] $ map collatzLength [1..1000000]