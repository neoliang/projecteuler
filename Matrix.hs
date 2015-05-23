module Matrix where

import Data.Char
type Matrix = [[Int]]

s2i :: String -> Int
s2i = read

fromString :: String -> Matrix
fromString string = let strMatrix = (map words).lines $ string in map (map s2i) strMatrix

index m r c = m !! r !! c

maxRow :: Matrix -> Int
maxRow = length 
maxCol :: Matrix -> Int
maxCol = maximum . map length
maxDiag :: Matrix -> Int
maxDiag m =  maxRow m + maxCol m - 1

row::Matrix -> Int -> [Int]
row m r | 0 <= r && r < length m =  m !! r
        |otherwise         = []

col::Matrix -> Int -> [Int]

col m c | 0 <= c && c < maxCol m = map (!! c) m
        | otherwise = []

rows :: Matrix ->[[Int]]
rows m = m

cols :: Matrix ->[[Int]]
cols m = map (col m) [0..maxCol m -1] 


startRow m d = if d < maxRow m then d else maxRow m -1
startCol m d = if d < maxRow m then 0 else d - maxRow m + 1
forwardDiag :: Matrix ->Int -> [Int]
forwardDiag m d | d < 0 || d >= maxDiag m = []
                | otherwise = zipWith (index m) [sr,sr-1 ..0] [sc..min d (maxCol m -1)]
                  where sr = startRow m d 
                        sc = startCol m d
forwardDiags m = map (forwardDiag m) [0..maxDiag m -1]

transpos :: Matrix -> Matrix
transpos = cols
mirror :: Matrix -> Matrix
mirror = map reverse
backwardDiag = forwardDiag . mirror
backwardDiags m = map (backwardDiag m) [0..maxDiag m -1]