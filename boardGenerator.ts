// Copy the below into a javascript console.
// Then use generateBoard(Int, Int) to get string representation of a haskell [[Bool]]
// Copy this string value into a haskell file without the quotes and it should work
let generateBoard = (x, y) => {
  let numValuesInRows = Array.from(new Array(x).keys())
  let rowCount = Array.from(new Array(y).keys())
  let matrix = []

  rowCount.forEach(yIndex => {
    let values = []
    numValuesInRows.forEach(() => {
      let rand = Math.floor(Math.random() * 10) + 1
      // Change the int below to anything between 1-10. This is the chance
      // that the value will be true. Int = 6 => True = 60%, False = 40%
      values.push(rand < 6)
    })
    matrix[yIndex] = values
  })

  let printMatrix = () => {
    let finalStr = ''
    matrix.forEach((row) => {
      let rowStr = ''
      let i = 0
      row.forEach((status) => {
        if (status && i < (row.length-1)) rowStr += 'True,'
        else if (status) rowStr += 'True'
        else if (!status && i < (row.length-1)) rowStr += 'False,'
        else if (!status) rowStr += 'False'
        i++
      })
      finalStr += '[' + rowStr + '],'
    })
    return finalStr
  }
  let preFinal = '[' + printMatrix() + ']'
  return preFinal.substring(0, (preFinal.length-2)) + ']'
}
