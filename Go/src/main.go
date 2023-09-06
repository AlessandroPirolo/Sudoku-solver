package main

import (
	"fmt"
	"time"
	"flag"
	"github.com/AlessandroPirolo/Sudoku-solver/sudoku"
)

func main() {

	board := flag.String("path", "", "")
	
	flag.Parse()

	path := string(*board)
	
	mySudoku := sudoku.Load(path)

	mySudoku.Print()

	start := time.Now()

	sudokuSolved := sudoku.Solve(mySudoku)

	finished := time.Since(start)

	fmt.Println("\nSolution:")
	sudokuSolved.Print()
	fmt.Println("\nSudoku solved in", finished)

}
