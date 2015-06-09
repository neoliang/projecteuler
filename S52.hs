
module Main where
import Data.List(sort)
main = print s52

sameDig a b = sort (show a) == sort (show b)

same x = all (\n->sameDig x (n*x) ) [2..6]

s52 = head $ filter same [1..]