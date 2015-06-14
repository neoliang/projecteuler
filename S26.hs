
module Main where
import Data.List(maximumBy)
import Data.Function(on)
main = print s26

fractCalc :: (Int,Int)-> (Int,Int)
fractCalc (n, d)
  | n >= d = (n `div` d,(n `mod` d) *10)
  | otherwise = (0,n*10)

fract ::Int->Int->[(Int,Int)]
fract n d
  | remain == 0 = [(n,divi)]
  | otherwise = (n,divi) :fract remain d  where
    (divi,remain) = fractCalc (n, d)

franctionPart n d = let parts = fract n d in recurringCyle $ take 1000 parts

recurringCyle [] = []
recurringCyle (x:xs)
 | x `elem` xs = map snd $ x: takeWhile (/=x) xs 
 | otherwise = recurringCyle xs

allcycle = map (franctionPart 1) $ [1..1000]
s26 = snd.maximumBy (compare `on` length.fst ) $ zip allcycle [1..1000]