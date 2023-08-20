-module(main).

-export([start/1]).

start(FilePath) ->

  Problem = sudoku_file:load(FilePath),
  sudoku:print_sudoku(Problem),
  % Check if the problem is already solved
  case sudoku:is_solved(Problem) of
    true ->
      %io:format("~p~n", [sudoku:is_solved(Problem)]),
      sudoku:print_sudoku(Problem);
    false ->
      % Initialize the board
      InitialSudoku = sudoku:init(Problem),

      % Map and reduce
      SudokuReduced = sudoku:map_and_reduce(InitialSudoku),

      % Recursive function
      {Row, Col} = sudoku_solver:search_first_empty(SudokuReduced),

      Candidates = maps:keys(sudoku:get_candidate(SudokuReduced, Row, Col)),

      PidS = lists:map(fun(Num) ->
        spawn_link(sudoku_solver, solve, [SudokuReduced, {Row, Col}, Num, self()])
        end, Candidates),

      receive
        {not_valid, Pid, _} ->
          io:format("~p is done ~n", [Pid]),
          exit(Pid, "Sudoku board not valid ~n");
        {solved, Solution, SolPid} ->
          lists:foreach( fun(Pid) ->
           exit(Pid, "Solution found")
            end,PidS),
          case sudoku:is_solved(Solution) of
            true ->
              io:format("solution found by ~p~n", [SolPid]),
              sudoku:print_sudoku(Solution);
            false ->
              io:format("solution not found")
          end
      end

  end.


