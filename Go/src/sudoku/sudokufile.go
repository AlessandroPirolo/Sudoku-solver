package sudoku

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func toRow(line string) Row {

	// Remove white spaces and split the string
	lineTrimmed := strings.TrimSpace(line)
	newLine := strings.Split(lineTrimmed, ",")

	var row Row

	// Create a row
	for _, str := range newLine {

		num, err := strconv.Atoi(str)

		if err != nil {
			fmt.Printf("Error converting '%s' to int: %v\n", str, err)
			continue
		}

		row = append(row, num)
	}

	return row
}

func Load(fileName string) Sudoku {

	var sudoku Sudoku

	file, err := os.Open(fileName)

	// Check for errors during the opening of the file
	if err != nil {
		log.Fatal(err)
	}

	// Create a scanner to read the file line by line
	scanner := bufio.NewScanner(file)

	// Read the file line by line, transform it in a Row type, then insert it into sudoku
	for scanner.Scan() {

		line := scanner.Text() // Get the current line
		row := toRow(line)
		sudoku = append(sudoku, row)

	}

	// Check for any errors
	if err := scanner.Err(); err != nil {
		fmt.Println("Error scanning file:", err)
	}

	file.Close() // Close the file

	return sudoku
}
