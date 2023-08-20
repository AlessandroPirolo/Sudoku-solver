-module(sudoku_solver).

%% API
-export([solve/4]).

solve(SudokuBoard, {Row, Col}, Value, Father) ->

  IntSudoku = sudoku:set_cell(SudokuBoard, Row, Col, Value),

  case sudoku:is_solved(IntSudoku) of
    true ->
      Father ! {solved, IntSudoku};
    false ->
      case sudoku:is_valid(IntSudoku) of
        false ->
          %io:format("not valid ~n ~p ~n", [sudoku:is_valid(IntSudoku)]),
          Father ! {not_valid, self(), IntSudoku};
        true ->
          %io:format("valid so continue ~n"),
          continue(IntSudoku, Father)
      end
  end.

continue(SudokuBoard, Father) ->
  InitialSudoku = sudoku:init(SudokuBoard),

  % Map and reduce
  SudokuReduced = sudoku:map_and_reduce(InitialSudoku),

  case sudoku:is_solved(SudokuReduced) of
    true ->
      Father ! {solved, SudokuReduced};
    false ->
      case sudoku:is_valid(SudokuReduced) of
        false ->
          %io:format("not valid ~n ~p ~n", [sudoku:is_valid(IntSudoku)]),
          Father ! {not_valid, self(), SudokuReduced};
        true ->
          % Recursive function
          {Row, Col} = sudoku:search_first_empty(SudokuReduced),


          Candidates = maps:keys(sudoku:get_candidate(SudokuReduced, Row, Col)),

          %io:format("In continue ~p~n", [{{Row, Col}, Candidates}]),

          _PidS = lists:map(fun(Num) ->
            spawn_link(sudoku_solver, solve, [SudokuReduced, {Row, Col}, Num, Father])
                            end,Candidates)
      end
  end.


