-- test matrices
let m1 = [[False, True, True],[True, True, True],[False, False, False]]


-- Conway's Game of life
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

countAliveAdjacents oMatrix x y = length (filter
  (\(x, y) -> (y >= 0  && y < (length oMatrix) && x >= 0 && x < (length (oMatrix!!0)) && oMatrix!!y!!x))
  [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)])
