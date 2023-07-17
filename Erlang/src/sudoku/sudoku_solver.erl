-module(sudoku_solver).

%% API
-export([search_first_empty/1, solve/4, list_candidates/1]).

% Check for the first empty cell occurrence and return its position
search_first_empty(SudokuBoard) ->
  search_first_empty(SudokuBoard, 1).

search_first_empty(Sudoku, Row) ->
  case search_first_empty(Sudoku, Row, 1) of
    {Row, Col} ->
      {Row, Col};
    false ->
      search_first_empty(Sudoku, Row + 1)
  end;
search_first_empty(_, _) ->
  false.

search_first_empty([Row|Rest], RowNum, ColNum) ->
  Cell = lists:nth(Row, ColNum),
  case is_empty(Cell) of
    true ->
      {RowNum, ColNum};
    false ->
      search_first_empty([Row|Rest], RowNum, ColNum + 1)
  end;
search_first_empty(_, _, _) ->
  false.

% Check whether a cell is empty or not
is_empty(Cell) ->
  case Cell of
    Num when is_integer(Num) ->
      false;
    {'_', _} ->
      true;
    '_' ->
      true;
    _ ->
      error({invalid_cell, Cell})
  end.

list_candidates(Candidates) ->
  TrueCandidates = maps:filter( fun(_Key, Value) ->
    case Value of
      true ->
        true;
      _ ->
        false
    end
                                end,Candidates),

  TrueCandidatesList = maps:keys(TrueCandidates),
  TrueCandidatesList.

solve(SudokuBoard, {Row, Col}, Value, Father) ->
  % Check if the problem is already solved
  IntSudoku = sudoku:set_cell(SudokuBoard, Row, Col, Value),
  case sudoku:is_solved(IntSudoku) of
    true ->
      Father ! {solved, IntSudoku};
    false ->
      case sudoku:is_valid(IntSudoku) of
        false ->
          Father ! {not_valid, self()};
        true ->
          continue(IntSudoku, Father)
      end
  end.

continue(SudokuBoard, Father) ->
  InitialSudoku = sudoku:init(SudokuBoard),

  % Map and reduce
  SudokuReduced = sudoku:map_and_reduce(InitialSudoku),

  % Recursive function
  {Row, Col} = search_first_empty(SudokuReduced),

  Candidates = sudoku:get_candidate(SudokuReduced, Row, Col),

  TrueCandidates = list_candidates(Candidates),

  _PidS = lists:map(fun(Num) ->
    spawn_link(sudoku_solver, solve/4, [SudokuReduced, {Row, Col}, Num, Father])
                end,TrueCandidates).
