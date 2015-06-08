
module Main where
import System.Environment

main = do
  contents <- getContents
  let s13 =  take 10 .show . sum . digitList $ contents
  print s13
digitList ::String -> [Integer]
digitList = map read  . words

-- cat s13.txt | runhaskell S13.hs