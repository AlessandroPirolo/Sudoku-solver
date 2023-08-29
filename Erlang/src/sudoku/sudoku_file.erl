-module(sudoku_file).

% API
-export([load/1]).

load(FilePath) ->
  {ok, File} = file:read_file(FilePath),
  Board = parse_board(File),
  Board.

% Parse the board
parse_board(Bin) ->
  Rows = binary:split(Bin, <<"\n">>, [global]),
  Board = lists:droplast(lists:map(fun parse_row/1, Rows)),
  Board.

% Parse elements divide by "\n" and convert it into rows
parse_row(RowBin) ->
  Elements = binary:split(RowBin, <<",">>, [global]),
  lists:map(fun parse_element/1, Elements).

% Parse each element and convert it into a cell
parse_element(ElemBin) ->
  Str = binary_to_list(ElemBin),
  {Num, _} = string:to_integer(string:trim(string:strip(Str))),
  case Num of
    0 -> '_';
    Value when Value > 0 -> Num
  end.