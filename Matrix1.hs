module Matrix where

import qualified Data.Vector as V

data Matrix = FromVector (V.Vector (V.Vector Int))

instance Show Matrix where
  show (FromVector m) = foldl (\acc row -> if acc == "" then show .V.toList $ row else acc ++ "\n" ++ (show . V.toList $ row)) "" m

s2i ::String->Int
s2i = read

fromString' :: String -> [[Int]]
fromString' string = let strMatrix = (map words).lines $ string in map (map s2i) strMatrix

fromString :: String -> Matrix
fromString string = FromVector $ V.fromList $ map (V.fromList) $ fromString' string

index :: Matrix -> Int -> Int -> Int
index (FromVector m) r c = m V.! r V.! c

maxRow :: Matrix -> Int
maxRow (FromVector m) = length m
maxCol :: Matrix -> Int
maxCol (FromVector m) = minimum . V.map length $ m
maxDiag :: Matrix -> Int
maxDiag (FromVector m) =  maxRow (FromVector m) + maxCol (FromVector m) - 1

row::Matrix -> Int -> V.Vector Int
row (FromVector m) r = m V.! r

col::Matrix -> Int -> V.Vector Int

col (FromVector m) c | 0 <= c && c < maxCol (FromVector m) = V.map (V.! c) m
        | otherwise = V.fromList []

rows :: Matrix -> [[Int]]
rows (FromVector m) = map (V.toList.row (FromVector m)) [0..maxRow (FromVector m) -1]  

cols :: Matrix -> [[Int]]
cols (FromVector m) = map (V.toList.col (FromVector m)) [0..maxCol (FromVector m) -1] 


startRow (FromVector m) d = if d < maxRow (FromVector m) then d else maxRow (FromVector m) -1
startCol (FromVector m) d = if d < maxRow (FromVector m) then 0 else d - maxRow (FromVector m) + 1
forwardDiag :: Matrix ->Int -> [Int]
forwardDiag m d | d < 0 || d >= maxDiag m = []
                | otherwise = zipWith (index m) [sr,sr-1 ..0] [sc..min d (maxCol m -1)]
                  where sr = startRow m d 
                        sc = startCol m d
forwardDiags m = map (forwardDiag m) [0..maxDiag m -1]

transpos :: Matrix -> Matrix
transpos  = FromVector . V.fromList . map (V.fromList) .cols
mirror :: Matrix -> Matrix
mirror (FromVector m) = FromVector $ V.map V.reverse m
backwardDiag = forwardDiag . mirror
backwardDiags m = map (backwardDiag m) [0..maxDiag m -1]