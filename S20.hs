module S20 where
import Char

main = print s20

factorial n = product [1..n]

s20 = sum . map (toInteger.digitToInt) . show .factorial $ 100 