-module(sudoku_file).

% API
-export([load/1]).

load(PathName) ->
  {ok, File} = file:open(PathName, [read]),
  Board = array:new(9),
  Sudoku = load_lines(File, Board, 1),
  file:close(File),
  Sudoku.

load_lines(File, Board, Ind) when Ind =< length(Board) ->
  case file:read_line(File) of
    {ok, Line} ->
      NewLine = split(Line),
      Row = transform(NewLine),
      S = array:set(Ind, Row, Board),
      SudokuBoard = load_lines(File, S, Ind + 1),
      array:to_list(SudokuBoard);
    eof ->
      array:to_list(Board)
  end.

transform(Line) ->
  lists:map(fun(Elem) ->
    case Elem of
      0 ->
        '_';
      Num when Num > 0 ->
        Num
    end
    end,Line).

split(Line) ->
  List = string:split(Line, ",", all),
  unicode:characters_to_list(List).