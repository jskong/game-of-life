play :: [[Bool]] -> Int -> [[Bool]]
play matrix 0 = matrix
play matrix numTurns = play (runTurn matrix) (numTurns - 1)

runTurn :: [[Bool]] -> [[Bool]]
runTurn matrix = []
  -- for each cell c0 in the matrix:
  -- count the number of live neighbours n
  -- then applyRules c0 n

applyRules :: Bool -> Int -> Int
applyRules c0 n =
  if c0 == True
  then if (n == 3)
       then 1
       else 0
  else if (n == 2 || n == 3)
       then 1
       else 0

-- countAdjacents:: [[Bool]] -> Int -> Int -> IO Int
countAdjacents oMatrix x y = do
 let adjLocs = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
 let withinBorder = [(x, y) | (x, y) <- adjLocs, (\(x, y) -> if (y < 0 || y > ((length oMatrix)-1) || x < 0 || x > ((length (oMatrix!!0))-1)) then False else True) (x,y)]
 let aliveAdjs = [(x, y) | (x, y) <- withinBorder, (\(x, y) -> oMatrix!!y!!x) (x,y)]
 return (length aliveAdjs)
