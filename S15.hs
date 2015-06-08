module Main where
import Control.Monad
main = print s15


--- list all paths
--paths :: Int -> Int
--paths n = length $ paths' (0,0) (n,n) n

--paths' ::(Int,Int) -> (Int,Int) -> Int -> [[(Int,Int)]]
--paths' from to n
--  | from == to = [[to]]
--  | otherwise = [ from:p | p <- concatMap (\from' -> paths' from' to n ) $ rightDownPoses  from n]

--rightDownPoses (p,p') n = [(x,y) | (x,y)<- [(p+1,p'),(p,p'+1)] ,x <= n && y <= n]

---- list all paths 
--paths'' :: Int -> Int
--paths'' n = paths''' (0,0) (n,n) where
--  paths''' from to
--    | from == to = 1
--    | otherwise = sum . map (flip paths''' to) . rightDownPoses from $ n 


---- path length recursive 
--pathLength :: Integer -> Integer -> Integer
--pathLength m n
--  | m == 0 && m == 0 = 1
--  | m == 0 = pathLength (n-1) m
--  | n == 0 = pathLength n  (m-1)
--  | otherwise = pathLength (n - 1) m + pathLength n (m -1)




--path length iterate 
pathMatrix:: Int -> [[Integer]]

pathMatrix n =  reverse $ foldl gencols [] (replicate (n+1) 1) where
  gencols css col
    | null css = (replicate (n+1) 1) : css
    | otherwise = (gencol (head css) col) : css
  gencol cs col = reverse $ foldl (\cols' c -> if null cols' then c : cols' else c + head cols' : cols' ) [] cs

pathLength' n = last.last $ pathMatrix n 

s15 = pathLength' 20
