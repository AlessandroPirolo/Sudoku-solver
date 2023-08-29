-module(main).

-export([start/1]).

start(FilePath) ->

  % Load the board
  Problem = sudoku_file:load(FilePath),
  sudoku:print_sudoku(Problem),

  % Check if the problem is already solved
  case sudoku:is_solved(Problem) of
    true ->
      % Already solved: we print the solution
      sudoku:print_sudoku(Problem);
    false ->
      % Initialize the board
      InitialSudoku = sudoku:init(Problem),

      % Map and reduce
      SudokuReduced = sudoku:map_and_reduce(InitialSudoku),

      % Search the first empty cell
      {Row, Col} = sudoku:search_first_empty(SudokuReduced),

      % Get the candidate numbers of the first empty cell
      Candidates = maps:keys(sudoku:get_candidate(SudokuReduced, Row, Col)),

      % For any candidate number, start a process 
      _PidS = lists:map(fun(Num) ->
        spawn_link(sudoku_solver, solve, [SudokuReduced, {Row, Col}, Num, self()])
        end, Candidates),
        
      loop()
    
  end.


loop() -> 
  receive
    {not_valid, Pid} ->
      % The solution isn't valid so we are stucked, then kill the process
      exit(Pid, "Sudoku board not valid ~n"),
      loop();
    {solved, Solution, SolPid} ->
      % Solution found
      case sudoku:is_solved(Solution) of
        true ->
          % Print the solution
          io:format("solution found by ~p~n", [SolPid]),
          sudoku:print_sudoku(Solution);
        false ->
          loop()
      end
  end.
	
