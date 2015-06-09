
module Main where
import Data.List(sort,group)
main = putStrLn s29

comboNumber n  =  map head . group . sort $ [a^b  | a<-[2..n],b<-[2..n]]

s29 = length $ comboNumber 100

comboNumber' n = foldr genA [] [2..n] where
  genA a ls = foldr genB ls [2..n] where
    genB b ls = let r = a^b in if r `elem` ls then ls else r:ls