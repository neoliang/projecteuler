module S21 where

main = print s21

--solution 21
factors n = [ x | x<-[1..(n `div` 2)] , n `mod` x == 0]
d = sum . factors
amicable n = (n == d dn) && (n /= dn) where dn = d n

s21 = sum . (filter amicable) $ [1..10000]