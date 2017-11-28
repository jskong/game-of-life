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
    (zipWithIndex row))
  (zipWithIndex matrix)

applyRules :: Bool -> Int -> Bool
applyRules cellStatus numAliveAdj =
  if cellStatus
  then (numAliveAdj == 2 || numAliveAdj == 3)
  else (numAliveAdj == 3)

countAliveAdjacents :: [[Bool]] -> Int -> Int -> Int
countAliveAdjacents oMatrix x y = length (filter (isValidAliveCoordOf oMatrix) (adjacentCoordsOf x y))


-- Helpers
zipWithIndex = zip [0..]
adjacentCoordsOf x y = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
isValidAliveCoordOf matrix = (\(x, y) -> (y >= 0  && y < (length matrix) && x >= 0 && x < (length (matrix!!y)) && matrix!!y!!x))
