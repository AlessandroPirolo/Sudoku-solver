package sudoku

import (
	"os"
	"log"
	"fmt"
)

func Load(fileName string) {

	content, err := os.ReadFile(fileName)

	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(string(content))
}