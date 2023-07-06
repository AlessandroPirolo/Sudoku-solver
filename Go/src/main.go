package main

import (
	"github.com/AlessandroPirolo/Sudoku-solver/sudoku"
)

func main() {

	var mySudoku sudoku.Sudoku

	mySudoku = sudoku.Load("../example.txt")

	mySudoku.Print()
}
