divisorNumIter [] = []
divisorNumIter ((num,dsum):xs) = (num,dsum): map addNext (divisorNumIter xs) where
  addNext (nextNum,nextsum)
    | (nextNum `mod` num == 0 ) && (dsum + nextNum > nextsum) = (nextNum,dsum+nextNum) 
    | otherwise =  (nextNum,nextsum)
divisorNumSets = divisorNumIter $ zip [1..] (1:[0,0..])