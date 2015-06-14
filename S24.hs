module Main where

main = print s24

prem ::[Int] -> [[Int]]
prem [] = [[]]
prem xs = do
  x <- xs
  let ys = [y| y<-xs ,x /= y]
  xs' <- prem ys
  return (x:xs')

s24 = prem [0..9] !! (1000000-1)