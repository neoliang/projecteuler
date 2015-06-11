

module Main where

main = do 
  str <- readFile "p067_triangle.txt"  
  print . maximum . last . maxTriangleRoutes . trianglesFromString $ str

trianglesFromString :: String -> [[Integer]]
trianglesFromString str =  map (map read .words). lines $ str

maxTriangleRoutes ::[[Integer]] -> [[Integer]]
maxTriangleRoutes tss = reverse $ foldl  maxRow [] tss where
  maxRow accRows ts = genRow : accRows where
    genRow = reverse $ foldl maxCol [] (zip ts [0..])
    maxCol accCols (t,i)
      | null accRows = t:accCols
      | otherwise = maximum [up,upLeft]  : accCols where
        lastRow = head accRows
        lenLastRow = length lastRow
        up = if i < lenLastRow then (lastRow !! i) + t else 0
        upLeft = if i > 0 then lastRow !! (i-1) + t else 0