package main

import (
	"fmt"
	"time"

	"github.com/AlessandroPirolo/Sudoku-solver/sudoku"
)

func main() {

	mySudoku := sudoku.Load("../example.txt")

	mySudoku.Print()

	start := time.Now()

	sudokuSolved := sudoku.Solve(mySudoku)

	finished := time.Since(start)

	sudokuSolved.Print()
	fmt.Println("Sudoku solved in", finished)

}
