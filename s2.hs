
main = print (s2 4000000)
fibonacci :: Num a => a-> a
fibonacci x
	| x == 1 = 1
	| x == 2  = 2
	| otherwise = fibonacci (x-1) + fibonacci (x-2)
s2 n = sum.(filter even).takeWhile(<n).(map fibonacci)$[1..]