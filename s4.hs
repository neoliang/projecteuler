--solution 4
module Main where

main = print s4
isPalindrome::(Integral t)=> t->Bool
isPalindrome x = show x == reverse(show x)
s4 = maximum  [x*y| x <- reverse[100..999] ,y <- reverse[100..999],isPalindrome (x*y)]