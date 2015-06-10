module Main where
import Data.List(sort,group)

main = putStrLn "Hello World"

combo n r = (factor n) `div` ( factor r * factor (n-r)) where 
  factor n = product[1..n]

setcombs n =  concat $ foldr genMatrix [] [1..n] where
  genMatrix row matrix  = foldr genRow [] [1..n] : matrix where
    genRow col rows = if col <= row then combo row col : rows else rows
s53 = length .filter (>1000000) $ setcombs 100