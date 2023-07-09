package sudoku

import "fmt"

// Rows are arrays of integers
type Row []int

// A sudoku board is an array of Row
type Sudoku []Row

// Every position in the board has a map associated, saying if a number can be used to fill that position or not
type CandidateNumbers map[int]bool

// Cell represents a position in the Sudoku board
type Cell struct {
	Row              int
	Col              int
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

	for _, Row := range sudoku {

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

func (sudoku Sudoku) IsValid() bool {
	columns := sudoku.getColumns()
	boxes := sudoku.getBoxes()

	for _, Row := range sudoku {
		if !(Row.repetitions()) {
			return false
		}
	}

	for _, Row := range columns {
		if !(Row.repetitions()) {
			return false
		}
	}

	for _, Row := range boxes {
		if !(Row.repetitions()) {
			return false
		}
	}

	return true
}

func (sudoku Sudoku) Copy() Sudoku {

	newSudoku := make(Sudoku, 0)

	for _, row := range sudoku {

		newRow := make(Row, 0)

		for _, num := range row {
			newRow = append(newRow, num)
		}

		newSudoku = append(newSudoku, newRow)
	}

	return newSudoku
}

// Update a position in the sudoku board with a value
func (sudoku Sudoku) Update(cell Cell, n int) {

	if n != -1 {
		sudoku[cell.Row][cell.Col] = n
	}

}

// This does the following:
//   - set the candidates number for every cell
//   - get a list of cells who have just one candidate number
//   - fill those cells
func (sudoku Sudoku) MapAndReduce() {

	cells := make([]Cell, 0)

	for rowPos, row := range sudoku {

		for colPos := range row {

			if sudoku[rowPos][colPos] == 0 {

				cell := sudoku.setCandidateNumbers(rowPos, colPos)
				cells = append(cells, cell)

			}

		}

	}

	singleCandidateCells := getSingleCandidateNumberCell(cells)

	for _, cell := range singleCandidateCells {

		n := cell.getSingleNumber()
		sudoku.Update(cell, n)

	}
}

func getSingleCandidateNumberCell(cells []Cell) []Cell {

	singleCells := make([]Cell, 0)

	for _, cell := range cells {

		m := make(map[bool]int)

		for _, value := range cell.CandidateNumbers {
			m[value] = m[value] + 1
		}

		if m[true] == 1 {
			singleCells = append(singleCells, cell)
		}

	}
	return singleCells

}

// Taking a candidate number set of a single cell and starting from a default map,
// it sets to false those numbers which are not proper candidates
func (sudoku Sudoku) setCandidateNumbers(rowPos int, colPos int) Cell {

	boxPos := getBoxPosition(colPos, rowPos)
	candidateMap := defaultMap()
	columns := sudoku.getColumns()
	boxes := sudoku.getBoxes()

	row := sudoku[rowPos]
	column := columns[colPos]
	box := boxes[boxPos]

	for _, n := range row {
		if n > 0 {
			candidateMap[n] = false
		}
	}

	for _, n := range column {
		if n > 0 {
			candidateMap[n] = false
		}
	}

	for _, n := range box {
		if n > 0 {
			candidateMap[n] = false
		}
	}

	return Cell{Row: rowPos, Col: colPos, CandidateNumbers: candidateMap}

}

// Create a default map of candidate numbers, i.e. a map where all numbers are possible candidates
func defaultMap() CandidateNumbers {

	m := make(CandidateNumbers)

	for i := 1; i <= 9; i++ {
		m[i] = true
	}
	return m
}

func (cell Cell) getSingleNumber() int {

	n := -1

	for key, val := range cell.CandidateNumbers {

		if val {
			n = key
		}

	}

	return n

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

	m := make(map[int]int)

	for _, num := range row {

		if num > 0 {
			m[num] = m[num] + 1
		}

		if m[num] > 1 {
			return false
		}

	}

	return true
}

// Get a map of the columns
func (sudoku Sudoku) getColumns() map[int]Row {

	columns := make(map[int]Row)

	for _, Row := range sudoku {

		for colPos, num := range Row {

			columns[colPos] = append(columns[colPos], num)

		}
	}

	return columns
}

// Get a map of the boxes
func (sudoku Sudoku) getBoxes() map[int]Row {
	boxes := make(map[int]Row)

	for rowPos, Row := range sudoku {

		for colPos, num := range Row {

			boxPos := getBoxPosition(colPos, rowPos)
			boxes[boxPos] = append(boxes[boxPos], num)
		}
	}

	return boxes
}
