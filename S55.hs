module Main where

main = print s55

isPalindrome n = show n == (reverse $ show n)
fiftyIter n = take 50 $ drop 1 $ iterate (\x -> x + (read . reverse . show $ x) ) n
isLychrel n =  null .filter isPalindrome $ fiftyIter n 

lychres = filter isLychrel [1..10000]
s55 = length lychres
  