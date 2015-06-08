module Main where
import Data.Char
import Data.List(sort)
main = do
  contents <- fmap (flip splitString ',') getContents
  let names = sort $ map toString contents
  print . sum . zipWith (*) [1..] . map nameScore $ names

toString :: String -> String
toString = read
nameScore :: String -> Integer
nameScore  = sum . map toScore 

toScore::Char->Integer
toScore c =
  toInteger (ord c - ord 'A' + 1)
splitString [] _ = []
splitString (x:xs) d
  | d == x = splitString xs d
splitString str d = let (h,t) = break (==d) str in h : splitString t d

--cat names.txt | runhaskell S22.hs