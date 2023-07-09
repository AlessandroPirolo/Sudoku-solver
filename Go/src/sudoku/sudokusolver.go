package sudoku

import (
	"sync"
)

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
	solution, _ := solvePar(sudokuOut, cellList)

	return solution
}

// Solve the sudoku in a concurrent way. It takes a sudoku board and the
// list of unfilled cell as input
func solvePar(sudoku Sudoku, cells []Cell) (Sudoku, bool) {

	// At every recursive call, I reduce cells
	if len(cells) == 0 {
		return sudoku, sudoku.IsSolved()
	}

	// Takes the cell to evaluate
	cellToEvaluate := cells[0]

	// Creates a channel where to collect all the results
	chanSudokuSolve := make(chan Channel)
	// Creates a wait group that waits for every goroutine to finish
	wg := new(sync.WaitGroup)

	// For every candidate number with value true, update the sudoku
	// So every time, it generates multiple incomplete solutions that run concurrently
	for num, b := range cellToEvaluate.CandidateNumbers {

		wg.Add(1)

		// Call the solve function recursively, but as a go routine thread so that it executes asynchronously
		go func(sudokuIn Sudoku, cell Cell, fillVal int, val bool, wg *sync.WaitGroup, c *chan Channel) {

			defer wg.Done()

			if val {

				sudokuOut := sudoku.Copy()

				sudokuOut.Update(cell, fillVal)

				newCellList := make([]Cell, len(cells)-1) // I'm removing the head of the list because at
				copy(newCellList, cells[1:])              // the next call, I don't need it anymore

				// Recursice call with a new shorter list of cell to complete
				solutionInter, solved := solvePar(sudokuOut, newCellList)

				*c <- Channel{Intermediate: solutionInter, Solved: solved}

			}
		}(sudoku, cellToEvaluate, num, b, wg, &chanSudokuSolve)
	}

	// Wait until every routine complited and close the channel
	go func(wg *sync.WaitGroup, c chan Channel) {
		wg.Wait()
		close(c)
	}(wg, chanSudokuSolve)

	// Search for the solution: traverse the channel until it finds a valid solution
	for sol := range chanSudokuSolve {

		if sol.Solved {
			return sol.Intermediate, sol.Solved
		}
	}

	return sudoku, sudoku.IsSolved()
}
