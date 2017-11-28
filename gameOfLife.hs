-- test matrices
m1 = [[False, True, True],[True, True, True],[False, False, False]]


-- Conway's Game of life
play :: [[Bool]] -> Int -> [[Bool]]
play matrix 0 = matrix
play matrix numTurns = play (runTurn matrix) (numTurns - 1)

runTurn :: [[Bool]] -> [[Bool]]
runTurn matrix =
  map (\(yIndex, row) ->
    map (\(xIndex, cellStatus) ->
      applyRules cellStatus (countAliveAdjacents matrix xIndex yIndex))
    (enumerate row))
  (enumerate matrix)

applyRules :: Bool -> Int -> Bool
applyRules c0 n =
  if c0
  then if (n == 2 || n == 3)
       then True
       else False
  else if (n == 3)
       then True
       else False

countAliveAdjacents :: [[Bool]] -> Int -> Int -> Int
countAliveAdjacents oMatrix x y = length (filter (isValidCoord oMatrix) (adjacentCoordsOf x y))


-- Helpers
enumerate = zip [0..]
adjacentCoordsOf x y = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
isValidCoord matrix = (\(x, y) -> (y >= 0  && y < (length matrix) && x >= 0 && x < (length (matrix!!0)) && matrix!!y!!x))
