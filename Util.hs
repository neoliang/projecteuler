module Util where

map2 :: (a->b->c) ->[a] ->[b] -> [c]
map2 f as bs = concat $ foldr genMatrix [] as where
  genMatrix row matrix  = foldr genRow [] bs : matrix where
    genRow col rows = f row col : rows 

todigits :: Integer ->[Integer]
todigits n = map (read.(:[])) $ show n