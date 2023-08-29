-module(sudoku_solver).

%% API
-export([solve/4]).

solve(SudokuBoard, {Row, Col}, Value, Father) ->

  % Update the cell
  IntSudoku = sudoku:set_cell(SudokuBoard, Row, Col, Value),
  
  % Check whether it's solved or not
  case sudoku:is_solved(IntSudoku) of
    true ->
      Father ! {solved, IntSudoku, self()};
    false ->
      case sudoku:is_valid(IntSudoku) of
        false ->
          Father ! {not_valid, self()};
        true ->
          continue(IntSudoku, Father)
      end
  end.

continue(SudokuBoard, Father) ->

  % Update the board
  InitialSudoku = sudoku:init(SudokuBoard),
  SudokuReduced = sudoku:map_and_reduce(InitialSudoku),

  % Check whether it's solved or not
  case sudoku:is_solved(SudokuReduced) of
    true ->
      Father ! {solved, SudokuReduced, self()};
    false ->
      case sudoku:is_valid(SudokuReduced) of
        false ->
          Father ! {not_valid, self()};
        true ->
          % Search the first empty cell and get its candidate numbers 
          {Row, Col} = sudoku:search_first_empty(SudokuReduced),
          Candidates = maps:keys(sudoku:get_candidate(SudokuReduced, Row, Col)),

          % For any candidate number, start a process 
          _PidS = lists:map(fun(Num) ->
            spawn_link(sudoku_solver, solve, [SudokuReduced, {Row, Col}, Num, Father])
                            end,Candidates)
      end
  end.




