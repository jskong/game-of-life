play:: [[Int]] -> Int -> [[Int]]
play matrix 0 = matrix
play matrix numTurns = play(runTurn(maxtrix), numTurns-1)


runTurn matrix =
