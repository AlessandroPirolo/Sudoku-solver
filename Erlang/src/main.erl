-module(main).

-export([start/1]).

start(Problem) ->
  % Check if the problem is already solved
  case sudoku:is_solved(Problem) of
    true ->
      Problem;
    false ->
      % Initialize the board
      InitialSudoku = sudoku:init(Problem),

      % Map and reduce
      SudokuReduced = sudoku:map_and_reduce(InitialSudoku),

      % Recursive function
      {Row, Col} = sudoku_solver:search_first_empty(SudokuReduced),

      Candidates = sudoku:get_candidate(SudokuReduced, Row, Col),

      TrueCandidates = sudoku_solver:list_candidates(Candidates),

      PidS = lists:map(fun(Num) ->
        spawn_link(sudoku_solver, solve/4, [SudokuReduced, {Row, Col}, Num, self()])
        end,TrueCandidates),

      receive
        {not_valid, Pid} ->
          exit(Pid, "Sudoku board not valid");
        {solved, Solution} ->
          lists:foreach( fun(Pid) ->
            exit(Pid, "Solution found")
            end,PidS),
          sudoku:print_sudoku(Solution)
      end

  end.


