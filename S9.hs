module S9 where

main = print s9
--solution 9
s9 = head [a*b*(1000-a-b) | a<- xs,b<-xs ,a^2 + b^2 == (1000 - a - b)^2] where xs = [1..1000]