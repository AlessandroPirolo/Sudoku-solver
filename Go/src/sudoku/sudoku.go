package sudoku

import "ftm"

// Rows are arrays of integers
type Row []int

// A sudoku board is an array of Row 
type Sudoku []Row

// Every position in the board has a map associated, saying if a number can be used to fill that position or not
type CandidateNumbers map[int]bool

// Cell represents a position in the Sudoku board
type Cell struct {
	Row int
	Col int
	CandidateNumbers CandidateNumbers
}

// Print function
func (s Sudoku) Print() {
	for i, col := range s {
		fmt.Println(i, col)
	}
}

/**
 *	Check if the Sudoku is solved or not. For every row, it does the following:
 * 		- check if there are any zero or if there are repetitions of numbers;
 *		- create the columns, i.e. a row containing elements from the same column position but in different rows;
 *		- create the boxes, i.e. value in different rows and column. Sudoku has 9 boxes, each 3x3;
 *		- for both boxes and columns, repeat the operations done for the row (complete and repetitions)
*/
func (sudoku Sudoku) IsSolved() bool {
	columns := sudoku.getColumns()
	boxes := sudoku.getBoxes()

	for rowPos, Row := range sudoku {

		if !(Row.complete() && Row.repetitions()) {
			return false
		}

	}

	for _, Row := range columns {
		if !(Row.complete() && Row.repetitions()) {
			return false
		}
	}

	for _, Row := range boxes {
		if !(Row.complete() && Row.repetitions()) {
			return false
		}
	}

	return true
}

func (sudoku Sudoku) MapAndReduce() {
	
	
} 

func (sudoku Sudoku) getSingleCandidateNumberCell() []Cell {

	cells []Cell := []

	for rowPos, Row := range sudoku {
		
		for colPos, num := range Row {

			if num == 0 {

				candidates :=  
				for _, b := 

			}
		}
	}
}

// Taking a candidate number set of a single cell and starting from a default map, 
// it sets to false those numbers which are not proper candidates
func (sudoku Sudoku) setCandidateNumbers(rowPos int, colPos int) {

	boxPos = getBoxPosition(colPos, rowPos)
	baseCandidateMap := defaultMap()
	columns := sudoku.getColumns()
	boxes := sudoku.getBoxes()

	row := sudoku[rowPos]
	column := columns[colPos]
	box := boxes[boxPos]

	for _,n := range row {
		if n > 0 {
			baseCandidateMap[n] = false
		}
	}

	for _,n := range column {
		if n > 0 {
			baseCandidateMap[n] = false
		}
	}

	for _,n := range box {
		if n > 0 {
			baseCandidateMap[n] = false
		}
	}

}

// Create a default map of candidate numbers, i.e. a map where all numbers are possible candidates
func defaultMap() CandidateNumbers {
	
	m := make(CandidateNumbers)

	for i := 1; i <= 9; i++ {
		m[i] = true
	} 
	return m
}



// Given the coordinates of a cell in the board, it return the box in which it's contained
func getBoxPosition(col int, row int) int {

	pos := 0

	switch {
	case row < 3: 
		switch {
			case col < 3: 
					pos = 0
					break 
			case col < 6:
					pos = 1
					break
			case col < 9:
					pos = 2
					break
		}
		break
	case row < 6: 
		switch {
			case col < 3: 
					pos = 3
					break 
			case col < 6:
					pos = 4
					break
			case col < 9:
					pos = 5
					break
		}
		break
	case row < 9: 
		switch {
			case col < 3: 
					pos = 6
					break 
			case col < 6:
					pos = 7
					break
			case col < 9:
					pos = 8
					break
		}
		break
		
	}

	return pos
}

// Check whether a row, column or box has cell that contains zero or not
func (row Row) complete() bool {
	for _, n := range row {
		if n == 0 {
			return false
		}
	}
	return true
}

// Check whether a row, column or box has repetitions of numbers or not
func (row Row) repetitions() bool {
	m = make(map[int]int)
	for _,num := range row {
		m[num] = m[num] + 1
		if m[num] > 1 {
			return false
		}
	}
	return true
}

// Get a map of the columns
func (sudoku Sudoku) getColumns() [int]Row {

	columns := make(map[int]Row)
	
	for rowPos, Row := range sudoku {

		for colPos, num := range Row {
			
			columns[colPos] = append(columns[colPos], num)

		}
	}

	return columns
}

// Get a map of the boxes
func (sudoku Sudoku) getBoxes() [int]Row {
	boxes := make(map[int]Row)

	for rowPos, Row := range sudoku {

		for colPos, num := range Row {
			
			boxPos = getBoxPosition(colPos, rowPos)
			boxes[boxPos] = append(boxes[boxPos], num)
		}
	}

	return boxes
}

