-module(sudoku).

%% API
-export([is_solved/1, init/1, get_candidate/1, get_cell/3, set_cell/2, map_and_reduce/1, print_sudoku/1]).

% Type specification for the list of candidate numbers
-type candidate() :: map(integer(), boolean()).

% Type specification for a Sudoku cell
-type cell() :: integer() | {'_', candidate()} | '_'.

% Type specification for a Sudoku board
-type sudoku() :: [[cell()]].

% Print a sudoku board
print_sudoku(Sudoku) ->
  lists:foreach(fun(Row) -> print_row(Row) end, Sudoku).

print_row(Row) ->
  lists:foreach(fun(Cell) -> print_cell(Cell) end, Row),
  io:format("~n").

print_cell(Cell) ->
  case Cell of
    _ -> io:format(" _");
    Value -> io:format(" ~w", [Value])
  end.


% Get the value of a cell
get_cell(Sudoku, Row, Col) ->
  V = lists:nth(Row, Sudoku),
  Cell = lists:nth(Col, V),
  get_cell(Cell).

get_cell(Cell) ->
  case Cell of
    Num when is_integer(Num) ->
      Num;
    {'_', _} ->
      '_';
    _ ->
      error({invalid_cell, Cell})
  end.

% Set the value of a cell
set_cell(Cell, Value) ->
  case Cell of
    Num when is_integer(Num) ->  % The cell already has the desired value, return as is
      Cell;
    {'_', _} ->  % Cell with candidate map
      Value;  % Assuming Value is a valid integer
    _ ->  % Cell with a filled value
      {'_', #{}}
  end.

% Get the list of candidate numbers
get_candidate(Cell) ->
  case Cell of
    _ ->
      error({invalid_cell, Cell});
    {'_', Candidates} ->
      Candidates
  end.

% Initialization: in every empty list sets the candidate list
init(Sudoku) ->
  [[init_row(Sudoku, Row) || Row <- Sudoku] || _ <- lists:seq(1, length(Sudoku))].

% Initialization of one row
init_row(Sudoku, Row) ->
  lists:map(fun(Ind, Item) -> transform(Sudoku, Row, Ind, Item) end, Row).


predicate(Cell) ->
  case Cell of
    Num when is_integer(Num) ->
      false;
    '_' ->
      true;
    {'_', _} ->
      false
  end.

% Transform each cell of the form '_' in {'_', candidates}
transform(Sudoku, Row, Col, Cell) ->
  case predicate(Cell) of
    true  -> {_, set_candidate(Sudoku, Row, Col)};
    false -> Cell
  end.

% Set candidate list for a cell
set_candidate(Sudoku, Row, Col) ->
  CMap = maps:new(),
  lists:foreach(fun(I) ->
    Value = search_for(Sudoku, Row, Col, I),
    maps:put(I, not(Value), CMap)
                end, lists:seq(1, 9)).

% Checking if the sudoku board is solved
is_solved(Sudoku) -> complete(Sudoku) and repetitions(Sudoku).

% Check if there are no zero in the sudoku board
complete(Sudoku) -> complete(Sudoku, 1).

% Given a row, check if there are no zero
complete(Board, Row) when Row =< length(Board) ->
  case check_row(Board, Row, 1) of
    true ->
      complete(Board, Row + 1);
    false ->
      true
  end;
complete(_, _) ->
  false.

% Check if there are zero in a row
check_row([Row|Rest], RowNum, Col) when Col =< length(Row) ->
  case check_cell(Row, Col) of
    0 ->
      true;
    _ ->
      check_row([Row|Rest], RowNum, Col + 1)
  end;
check_row(_, _, _) ->
  false.

% Given the indexes, get an item from a list
check_cell(Row, Col) -> lists:nth(Col, Row).

% Check if there are repetitions in the rows, columns or boxes
repetitions(Sudoku) -> row_repetitions(Sudoku,1) and col_repetitions(Sudoku, 1) and box_repetitions(Sudoku, 1).

% Check whether there are repetitions in the same box
box_repetitions(Sudoku, Row) ->
  New = to_box(Sudoku),
  row_repetitions(New, Row).

% Get all nine 3x3 boxes in a sudoku board
to_box(Sudoku) ->
  [box(Sudoku, Row, Column) || Row <- [1, 4, 7], Column <- [1, 4, 7]].

box(Sudoku, StartRow, StartColumn) ->
  [lists:sublist(Row, StartColumn, 3) || Row <- lists:sublist(Sudoku, StartRow, 3)].

% Check whether there are repetitions in the same column
col_repetitions(Sudoku, Row) ->
  New = to_col(Sudoku),
  row_repetitions(New, Row).

% Transform columns into rows
to_col([]) ->
  [];
to_col([[] | _]) ->
  [];
to_col(Sudoku) ->
  [[lists:nth(N, Row) || Row <- Sudoku] || N <- lists:seq(1, length(Sudoku))].

% Check if there are at least two equal number in the same row
row_repetitions(Sudoku, Row) when Row =< length(Sudoku) ->
  case check_rep_row(Sudoku, Row) of
    true ->
      row_repetitions(Sudoku, Row + 1);
    false ->
      true
  end;
row_repetitions(_, _) ->
  false.

check_rep_row(Sudoku, Row) ->
  check_rep_row(Sudoku, Row, 1, []).

check_rep_row(Sudoku, Row, Col, Seen) when Col =< length(Row) ->
  case check_rep_cell(Sudoku, Row, Col, Seen) of
    true ->
      check_rep_row(Sudoku, Row, Col + 1, Seen);
    false ->
      true
  end;
check_rep_row(_, _, _, _) ->
  true.

% Check whether a number was already seen in the list
check_rep_cell(Sudoku, Row, Col, Seen) ->
  Cell = lists:nth(Col, lists:nth(Row,Sudoku)),
  case Cell of
    '_' ->
      true; % Empty cell are skipped
    {'_', _} ->
      true; % Empty cell are skipped
    Num when is_integer(Num) ->
      case lists:member(Num, Seen) of
        true ->
          false; % Repetition found
        false ->
          check_rep_cell(Sudoku, Row, Col + 1, [Num|Seen]) % Repetition not found and we insert Num in the seen list
      end;
    _ ->
      false % Something which isn't a valid cell format
  end.

% Return true if a value is already present in the row, column and box of a given cell
search_for(Sudoku, Row, Col, Value) ->
  search_row(Row, Value) and
    search_col(Sudoku, Col, Value) and
    search_box(Sudoku, Row, Col, Value).

% Return true if the value is already in the row
search_row(Row, Value) -> lists:member(Value, Row).

% Return true if the value is already in the column
search_col(Sudoku, Col, Value) ->
  Row = lists:nth(Col, to_col(Sudoku)),
  lists:member(Value, Row).

% Return true if the value is already in the box
search_box(Sudoku, Row, Col, Value) ->
  Box = lists:nth(pos(Row,Col), to_box(Sudoku)),
  lists:member(Value, Box).

pos(Row, Col) ->
  case Row of
    Num when Num =< 3 ->
      case Col of
        N when N =< 3 ->
          1;
        N when N > 3 and N =< 6 ->
          2;
        N when N > 6 and N =< 9 ->
          3
      end;
    Num when Num > 3 and Num =< 6 ->
      case Col of
        N when N =< 3 ->
          4;
        N when N > 3 and N =< 6 ->
          5;
        N when N > 6 and N =< 9 ->
          6
      end;
    Num when Num > 6 and Num =< 9 ->
      case Col of
        N when N =< 3 ->
          7;
        N when N > 3 and N =< 6 ->
          8;
        N when N > 6 and N =< 9 ->
          9
      end
  end.

%
map_and_reduce(Sudoku) ->
  Singles = get_single_cell(Sudoku),
  lists:foreach(fun(_Ind, Cell) ->
    Candidates = get_candidate(Cell),
    Value = maps:get(true, Candidates),
    set_cell(Cell, Value)
    end,Singles).


% Return every cell that has one candidate number in the sudoku board
get_single_cell(Sudoku) ->
  lists:foreach( fun(_Ind, Row) -> lists:filter(get_single_cell_row(Row), Row) end, Sudoku).

% Return every cell that has one candidate number in the row
get_single_cell_row(Row) ->
  lists:filter( fun(_Ind, Item) ->
    Value = get_candidate(Item),
    case Value of
      {error, {invalid_cell, _}} ->
        false;
      _ -> single(Value)
    end
    end,Row).

% Return true if the cell has just one candidate number
single(Candidates) ->
  List = maps:values(Candidates),
  L = length([X || X <- List, X =:= true]),
  if
    L > 1 -> false;
    L == 1 -> true
  end.