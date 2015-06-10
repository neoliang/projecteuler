module Main where
import Util
main = print s56

googos n = map2 (^) [1..n] [1..n]

s56 = maximum . map sum . map todigits $ googos 100