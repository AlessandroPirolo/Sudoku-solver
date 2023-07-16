-module(sudokufile).

% API
-export([load/1]).

load(PathName) ->
  {ok, File} = file:open(PathName, [read]),
  Board = array:new(9),
  Sudoku = load_lines(File, Board, 1),
  file:close(File),
  Sudoku.

load_lines(File, Board, Ind) ->
  case file:read_line(File) of
    {ok, Line} ->
      NewLine = split(Line),
      Row = transform(NewLine),
      S = array:set(Ind, Row, Board),
      SudokuBoard = load_lines(File, S, Ind + 1),
      SudokuBoard;
    eof ->
      Board
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
  List = binary:bin_to_list(Line),
  lists:filter( fun(Elem) ->
    case Elem of
      Num when is_integer(Num) ->
        true;
      _ ->
        false
    end
    end,List).