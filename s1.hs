
main = do print s1
s1 = sum [x|x<-[1..999] , (x `mod` 5 == 0) || (x `mod` 3 == 0)]