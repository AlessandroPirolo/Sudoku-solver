# Table of Contents
[Sudoku-solver](#sudoku-solver)

[Implementation](#implementation)
  - [Go](#go)
  - [Erlang](#erlang)
    
[Instructions and Examples](#instructions-and-examples)

# Sudoku-solver

A parallelized Sudoku solver written in Go and Erlang.

## Implementation

The implemented algorithm is a sort of brute-force algorithm, but there is one difference, i.e. it's parallelized. 

More precisely, the algorithm works as follow:
1. Starting from an incomplete Sudoku board, it takes the first empty cell.
2. It fills it with every possible number for that cell, i.e. it creates a "branch" for every number that proceed in parallel.
3. For any of these "braches", check if the inserted number is valid (i.e. it does not violate the rules of sudoku):
   - If the number is valid, it proceeds to the next empty cell and recursively repeats steps 1-3.
   - If the number is not valid, it ignores the "branch".
4. It repeats the procedure until there are no more empty cells on the board, which means it has found the solution.

This is the idea behind the algorithm. Since the procedure generates a exponential number of recursive call, what I actually did was making some adjustment in order to make the algorithm slightly more efficient. 

### Go 

Go implementation makes use of channels and go routines to solve a Sudoku puzzle:
- Go routines correspond to the so called "branches" which I've nominated earlier.
- Channels are used to collect every solution (also the wrong ones) so that, at the end of the computation, we can retrieve the solution.
The implementation follows the items above, but every time it does item (2), it tries to fill those cells which have only one possible acceptable number in order to reduce the number of recursive call. 

### Erlang

Erlang implementation uses actors to solve a Sudoku puzzle and, similarly to Go, it tries to fill those cells which have only one possible acceptable number at the beginning of the computation. Furthermore, every time a process gets stuck, that process is killed for the purpose of reducing the number of processes working at the same moment.

## Instructions and Examples

The versions of the solver work with files of this form:

~~~
8,0,0,0,0,0,0,0,0
0,0,3,6,0,0,0,0,0
0,7,0,0,9,0,2,0,0
0,5,0,0,0,7,0,0,0
0,0,0,0,4,5,7,0,0
0,0,0,1,0,0,0,3,0
0,0,1,0,0,0,0,6,8
0,0,8,5,0,0,0,1,0
0,9,0,0,0,0,4,0,0
~~~

So, commas are used to separate each cell and starting a new line is used to separate rows.

In Go, we start by simply compile the file inside sudoku folder, i.e:

`` $ go build sudoku/sudoku.go sudoku/sudokufile.go sudoku/sudokusolver.go ``

then, we compile the main:

`` $ go build main.go ``

And then, we specify the file of the puzzle by passing it as a command-line argument:

`` ./main -path="here the path to the file"``

The result will be like this:

~~~ 
  0 [8 0 0 0 0 0 0 0 0]
  1 [0 0 3 6 0 0 0 0 0]
  2 [0 7 0 0 9 0 2 0 0]
  3 [0 5 0 0 0 7 0 0 0]
  4 [0 0 0 0 4 5 7 0 0]
  5 [0 0 0 1 0 0 0 3 0]
  6 [0 0 1 0 0 0 0 6 8]
  7 [0 0 8 5 0 0 0 1 0]
  8 [0 9 0 0 0 0 4 0 0]

  Solution:
  0 [8 1 2 7 5 3 6 4 9]
  1 [9 4 3 6 8 2 1 7 5]
  2 [6 7 5 4 9 1 2 8 3]
  3 [1 5 4 2 3 7 8 9 6]
  4 [3 6 9 8 4 5 7 2 1]
  5 [2 8 7 1 6 9 5 3 4]
  6 [5 2 1 9 7 4 3 6 8]
  7 [4 3 8 5 2 6 9 1 7]
  8 [7 9 6 3 1 8 4 5 2]
 
  Sudoku solved in 13.350885587s
~~~

In Erlang, we open the Erlang shell and we compile the files:

~~~
   1> c(main).
   {ok,main}
   2> code:add_patha("sudoku/").
   true
   3> c(sudoku_file).
   Recompiling /home/pirale/Documenti/Sudoku-solver/Erlang/src/sudoku/sudoku_file.erl
   {ok,sudoku_file}
   4> c(sudoku_solver).
   Recompiling /home/pirale/Documenti/Sudoku-solver/Erlang/src/sudoku/sudoku_solver.erl
   {ok,sudoku_solver}
   5> c(sudoku).  
   Recompiling /home/pirale/Documenti/Sudoku-solver/Erlang/src/sudoku/sudoku.erl
   {ok,sudoku}
~~~
Then, we call the start method:

`` 6> main:start("path to file"). ``

The result will be like this:

~~~
 1 3 9 _ _ 2 _ _ _
 _ _ _ _ _ _ _ _ _
 _ 8 5 _ _ 6 9 _ _
 _ 1 _ 8 _ 3 7 _ 9
 _ _ _ 7 _ 9 _ _ _
 2 _ 7 6 _ 1 _ 4 _
 _ _ 4 2 _ _ 5 6 _
 _ _ _ _ _ _ _ _ _
 _ _ _ 1 _ _ 2 8 4
solution found by <0.241.0>
time: 0.045618 seconds 
 1 3 9 4 8 2 6 7 5
 7 6 2 9 1 5 4 3 8
 4 8 5 3 7 6 9 1 2
 5 1 6 8 4 3 7 2 9
 3 4 8 7 2 9 1 5 6
 2 9 7 6 5 1 8 4 3
 9 7 4 2 3 8 5 6 1
 8 2 1 5 6 4 3 9 7
 6 5 3 1 9 7 2 8 4
ok

~~~   



