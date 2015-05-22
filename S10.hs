--solution 10

module S10 where

main = print s10

sieve (x:xs) = x: sieve (filter (\y -> rem y x /= 0) xs) 
primes = sieve [2..]
s10 = sum. takeWhile (<200000) $ primes