-module(sudoku).

%% API
-export([is_solved/1, is_valid/1, init/1, get_candidate/3, get_cell/3, set_cell/4, map_and_reduce/1, print_sudoku/1, search_for/4, to_box/1, set_candidate/3, search_first_empty/1]).


% Print a sudoku board
print_sudoku(Sudoku) ->
  lists:foreach(fun(Row) -> print_row(Row) end, Sudoku).

print_row(Row) ->
  lists:foreach(fun(Cell) -> print_cell(Cell) end, Row),
  io:format("~n").

print_cell(Cell) ->
  case Cell of
    Value when is_integer(Value) ->
      io:format(" ~w", [Value]);
    _ ->
      io:format(" _")
  end.

% Get the value of a cell
get_cell(Sudoku, Row, Col) ->
  V = lists:nth(Row, Sudoku),
  Cell = lists:nth(Col, V),
  ReturnValue = get_cell(Cell),
  {ReturnValue, Row, Col}.

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
set_cell(Sudoku, RowNum, ColNum, Value) ->
  Row = lists:nth(RowNum, Sudoku),
  Cell = lists:nth(ColNum, Row),

  case Cell of
    Num when is_integer(Num) ->  % The cell already has the desired value
      Sudoku;

    {'_', _} ->  % Cell with candidate map
      NewRow = set_nth(ColNum, Row, Value),
      NewSudoku = set_nth(RowNum, Sudoku, NewRow),
      NewSudoku;  % Assuming Value is a valid integer

    '_' ->  % Empty cell
      NewRow = set_nth(ColNum, Row, Value),
      NewSudoku = set_nth(RowNum, Sudoku, NewRow),
      NewSudoku;

    _ ->  % By default
      Sudoku
  end.

set_nth(1, [_|Rest], New) ->
  [New|Rest];
set_nth(I, [E|Rest], New) ->
  [E|set_nth(I-1, Rest, New)].

% Get the list of candidate numbers
get_candidate(Sudoku, RowNum, ColNum) ->
  Row = lists:nth(RowNum, Sudoku),
  Cell = lists:nth(ColNum, Row),
  case Cell of
    {'_', Candidates} ->
      Candidates;
    _ ->
      error({invalid_cell, Cell})
  end.

% Initialization: in every empty list sets the candidate list
init(Sudoku) ->
  %[[init_row(Sudoku, Row) || Row <- Sudoku] || _ <- lists:seq(1, length(Sudoku))].
  lists:map(fun({RowNum, Row}) ->
    init_row(Sudoku, Row, RowNum)
            end, lists:zip(lists:seq(1, length(Sudoku)), Sudoku)).

% Initialization of one row
init_row(Sudoku, Row, RowNum) ->
  lists:map(fun({Col, Item}) -> transform(Sudoku, RowNum, Col, Item) end, lists:zip(lists:seq(1, length(Row)), Row)).


predicate(Cell) ->
  case Cell of
    Num when is_integer(Num) ->
      false;
    '_' ->
      true;
    {'_', _} ->
      true
  end.

% Transform each cell of the form '_' in {'_', candidates}
transform(Sudoku, Row, Col, Cell) ->
  case predicate(Cell) of
    true  -> {'_', set_candidate(Sudoku, Row, Col)};
    false -> Cell
  end.

% Set candidate list for a cell
set_candidate(Sudoku, Row, Col) ->
  Keys = lists:seq(1,9),
  Values = lists:map( fun(V) ->
    not(search_for(Sudoku, Row, Col, V))
    end, Keys),
  CMap = maps:from_list(lists:zip(Keys, Values)),
  maps:filter(fun(_, V) ->
    V =:= true
    end,CMap).

% Return true if a value is already present in the row, column and box of a given cell
search_for(Sudoku, Row, Col, Value) ->
  search_row(Sudoku, Row, Value) or
    search_col(Sudoku, Col, Value) or
    search_box(Sudoku, Row, Col, Value).

% Return true if the value is already in the row
search_row(Sudoku, RowNum, Value) ->
  Row = lists:nth(RowNum, Sudoku),
  lists:member(Value, Row).

% Return true if the value is already in the column
search_col(Sudoku, Col, Value) ->
  Row = lists:nth(Col, to_col(Sudoku)),
  lists:member(Value, Row).

% Return true if the value is already in the box
search_box(Sudoku, Row, Col, Value) ->
  Box = lists:nth(pos(Row,Col), to_box(Sudoku)),
  lists:member(Value, Box).

% Checking if the sudoku board is solved
is_solved(Sudoku) -> complete(Sudoku) and is_valid(Sudoku).

% Check if the sudoku board is valid, i.e. if there are repetitions of numbers
is_valid(Sudoku) -> not(repetitions(Sudoku)).

% Check if there are no zero in the sudoku board
complete(Sudoku) -> complete(Sudoku, 1).

% Given a row, check if there are no zero
complete(Board, Row) when Row =< length(Board) ->
  case check_row(Board, Row, 1) of
    true ->
      false;
    false ->
      complete(Board, Row + 1)
  end;
complete(_, _) ->
  true.

% Check if there are zero in a row
check_row(Sudoku, RowNum, Col) when RowNum =< length(Sudoku) andalso Col =< length(Sudoku) ->
  Row = lists:nth(RowNum, Sudoku),
  case check_cell(Row, Col) of
    Num when is_integer(Num) ->
      check_row(Sudoku, RowNum, Col + 1);
    _ ->
      true
  end;
check_row(_, _, _) ->
  false.

% Given the indexes, get an item from a list
check_cell(Row, Col) -> lists:nth(Col, Row).

% Check if there are repetitions in the rows, columns or boxes
repetitions(Sudoku) -> row_repetitions(Sudoku,1) or col_repetitions(Sudoku, 1) or box_repetitions(Sudoku, 1).

% Check whether there are repetitions in the same box
box_repetitions(Sudoku, Row) ->
  New = to_box(Sudoku),
  row_repetitions(New, Row).

% Get all nine 3x3 boxes in a sudoku board
to_box(Sudoku) ->
  [box(Sudoku, Row, Column) || Row <- [1, 4, 7], Column <- [1, 4, 7]].

box(Sudoku, StartRow, StartColumn) ->
  Rows = lists:sublist(Sudoku, StartRow, 3),
  BoxRows = [lists:sublist(Row, StartColumn, 3) || Row <- Rows],
  Box = lists:flatten(BoxRows),
  Box.

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
      true;
    false ->
      row_repetitions(Sudoku, Row + 1)
  end;
row_repetitions(_, _) ->
  false.

check_rep_row(Sudoku, Row) ->
  check_rep_row(Sudoku, Row, [0]).

check_rep_row(Sudoku, Row, Seen) when Row =< length(Sudoku) ->
  case check_rep_cell(Sudoku, Row, 1, Seen) of
    true ->
      true;
    false ->
      check_rep_row(Sudoku, Row + 1, Seen)
  end;
check_rep_row(_, _, _) ->
  false.

% Check whether a number was already seen in the list
check_rep_cell(Sudoku, Row, Col, Seen) when Row =< length(Sudoku) andalso Col =< length(Sudoku) ->
  Cell = lists:nth(Col, lists:nth(Row,Sudoku)),
  case Cell of
    '_' ->
      check_rep_cell(Sudoku, Row, Col + 1, Seen); % Empty cell are skipped
    {'_', _} ->
      check_rep_cell(Sudoku, Row, Col + 1, Seen); % Empty cell are skipped
    Num when is_integer(Num) ->
      case lists:member(Num, Seen) of
        true ->
          true; % Repetition found
        false ->
          NewList = lists:append(Seen, [Num]), % Repetition not found and we insert Num in the seen list
          check_rep_cell(Sudoku, Row, Col + 1, NewList)
      end;
    _ ->
      false % Something which isn't a valid cell format
  end;
check_rep_cell(_,_,_,_) ->
  false.

pos(Row, Col) ->
  case Row of
    Num when Num =< 3 ->
      case Col of
        N when N =< 3 ->
          1;
        N when N > 3, N =< 6 ->
          2;
        N when N > 6, N =< 9 ->
          3
      end;
    Num when Num > 3, Num =< 6 ->
      case Col of
        N when N =< 3 ->
          4;
        N when N > 3, N =< 6 ->
          5;
        N when N > 6, N =< 9 ->
          6
      end;
    Num when Num > 6, Num =< 9 ->
      case Col of
        N when N =< 3 ->
          7;
        N when N > 3, N =< 6 ->
          8;
        N when N > 6, N =< 9 ->
          9
      end
  end.

% Find every empty cell which has only one possible candidate and
% fill it with that number
map_and_reduce(Sudoku) ->
  lists:map(fun(Row) ->
    map_and_reduce_row(Row)
    end,Sudoku).

map_and_reduce_row(Row) ->
  lists:map(fun(Elem) ->
    case Elem of
      Num when is_integer(Num) ->
        Num;
      {'_', Candidates} ->
        case single(Candidates) of
          true ->
            N = lists:nth(1, maps:keys(Candidates)),
            N;
            %maps:get(true, Candidates);
          false ->
            {'_', Candidates}
        end
    end
  end, Row).

% Return true if the cell has just one candidate number
single(Candidates) ->
  List = maps:values(Candidates),
  L = length(List),
  if
    L =/= 1 -> false;
    L == 1 -> true
  end.

% Check for the first empty cell occurrence and return its position
search_first_empty(SudokuBoard) ->
  search_first_empty(SudokuBoard, 1).

search_first_empty(Sudoku, Row) when Row =< length(Sudoku) ->
  case search_first_empty(Sudoku, Row, 1) of
    {Row, Col} ->
      {Row, Col};
    false ->
      search_first_empty(Sudoku, Row + 1)
  end;
search_first_empty(_, _) ->
  {0, 0}.

search_first_empty(Sudoku, RowNum, ColNum) when RowNum =< length(Sudoku) andalso ColNum =< length(Sudoku)->
  Cell = lists:nth(ColNum, lists:nth(RowNum, Sudoku)),
  case is_empty(Cell) of
    true ->
      {RowNum, ColNum};
    false ->
      search_first_empty(Sudoku, RowNum, ColNum + 1)
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