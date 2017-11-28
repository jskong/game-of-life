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
