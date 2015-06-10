
type Rat = (Integer,Integer)
addRat :: Rat-> Rat->Rat
addRat (a,b) (c,d) = (a*d + b*c, b*d)

srtPart2 (a,b) = let (c,d) =  addRat(a,b) (2,1) in (d,c)

p2s = iterate srtPart2 (1,2)
srts = map (addRat (1,1)) p2s

s57 = length . filter (\(num,den)-> (length .show) num > (length.show) den) .take 1000 $ srts