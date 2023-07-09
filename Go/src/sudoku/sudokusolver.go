package sudoku

func Solve(sudoku Sudoku) Sudoku {

	// Firstly, we fill the cell with just one candidate number
	sudoku.MapAndReduce()

	sudokuOut := sudoku.Copy()

	// Checking if MapAndReduce was sufficient to solve it
	if sudokuOut.IsSolved() {
		return sudokuOut
	}

	// Making a list of cell to be updated
	cellList := make([]Cell, 0)
	for rowPos, row := range sudokuOut {

		for colPos, num := range row {

			if num == 0 {
				cell := sudokuOut.setCandidateNumbers(rowPos, colPos)
				cellList = append(cellList, cell)
			}

		}
	}

	// Calling the recursive function that solve the sudoku
	return solveSeq(sudokuOut, cellList)

}

// Solve the sudoku in a serial manner. It takes a sudoku board and the
// list of unfilled cell as input
func solveSeq(sudoku Sudoku, cells []Cell) Sudoku {

	// At every recursive call, I reduce cells
	if len(cells) == 0 {
		return sudoku
	}

	// Takes the cell to evaluate
	cellToEvaluate := cells[0]

	// For every candidate number with value true, update the sudoku
	// So every time, It generates multiple incomplete solutions
	for num, b := range cellToEvaluate.CandidateNumbers {

		if b {
			_sudokuOut := sudoku.Copy()

			_sudokuOut.Update(cellToEvaluate, num)

			newCellList := make([]Cell, len(cells)-1) // I'm removing the head of the list because at
			copy(newCellList, cells[1:])              // the next call, I don't need it anymore

			// Recursice call with a new shorter list of cell to complete
			_solutionInter := solveSeq(_sudokuOut, newCellList)

			if _solutionInter.IsSolved() {
				return _solutionInter
			}
		}
	}

	return sudoku

}
