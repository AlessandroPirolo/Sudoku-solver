package sudoku

import (
	"sync"
)

func Solve(sudoku Sudoku) Sudoku {

	// Checking if it's already solved
	if sudoku.IsSolved() {
		return sudoku
	}

	sudokuOut := sudoku.Copy()

	// Creates the channel which will contain the solution and
	solutionChan := make(chan Channel)
	done := make(chan struct{})
	defer close(done)

	// Calling the recursive function that solve the sudoku
	go solvePar(sudokuOut, solutionChan, done)

	// Traverse the channel and find the solution
	for sol := range solutionChan {

		if sol.Solved {
			return sol.Solution
		}
	}

	return sudokuOut
}

// Solve the sudoku in a concurrent way. It takes a sudoku board and the
// list of unfilled cell as input
func solvePar(sudoku Sudoku, solutionChan chan Channel, done <-chan struct{}) {

	// Every time we try to fill the cell with just one candidate number
	sudoku.MapAndReduce()

	// Making a list of cell to be updated
	cellList := sudoku.EmptyCells()

	// At every recursive call, the number of cells to evalute reduces
	if len(cellList) == 0 {
		select {
		case <-done:
			return
		case solutionChan <- Channel{Solution: sudoku, Solved: sudoku.IsSolved()}:
		}
		return
	}

	cellToEvaluate := cellList[0]

	// Creates a wait group that waits for every goroutine to finish
	var wg sync.WaitGroup
	wg.Add(len(cellToEvaluate.CandidateNumbers))

	// For every candidate number with value true, update the sudoku
	// So every time, it generates multiple incomplete solutions that run concurrently
	for num, b := range cellToEvaluate.CandidateNumbers {

		// Run the function that updates the cell
		go func(sudokuIn Sudoku, cell Cell, fillVal int, val bool) {

			defer wg.Done()

			if val {

				sudokuOut := sudoku.Copy()

				sudokuOut.Update(cell, fillVal)

				if sudokuOut.IsValid() {
					// Recursice call
					solvePar(sudokuOut, solutionChan, done)

				}

			}
		}(sudoku, cellToEvaluate, num, b)
	}

	// Wait until every routine complited
	wg.Wait()
}
