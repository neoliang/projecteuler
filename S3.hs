module S3 where

main = print s3

isPrime ::(Integral t) => t-> Bool
isPrime x
	| x == 1 = True
	| x == 2 = True
	| x < 1 = False
	| otherwise = foldl(\acc e -> if not acc then acc else (x `mod` e) /= 0 ) True [2..(floor.sqrt.fromIntegral $ x)] 

factors ::(Integral t) => t ->[t]
factors x = reverse [y | y<-[1..(floor.sqrt.fromIntegral $ x) ],x `mod` y == 0]

s3 = head . filter isPrime .factors $ 600851475143