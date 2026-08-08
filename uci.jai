UciReadLine :: () -> string, bool
{
  #if OS == .WINDOWS stdin := GetStdHandle(STD_INPUT_HANDLE);

  builder: String_Builder;
  while true
  {
    c: u8;
    #if OS == .WINDOWS
    {
      bytes_read: u32;
      got_byte := ReadFile(stdin, *c, 1, *bytes_read, null) != 0 && bytes_read > 0;
    }
    else
    {
      got_byte := read(STDIN_FILENO, *c, 1) > 0;
    }
    if !got_byte return "", false; // EOF or error
    if c == #char "\n" break;
    if c == #char "\r" continue;
    append(*builder, c);
  }
  return builder_to_string(*builder), true;
}

UciMoveFromString :: (text: string) -> Move, Piece
{
  move: Move;
  promotion: Piece;
  assert(text.count >= 4);

  fx := text[0] - #char "a";
  fy := text[1] - #char "1";
  tx := text[2] - #char "a";
  ty := text[3] - #char "1";
  move.from = xx ((7 - fy) * 8 + fx);
  move.to   = xx ((7 - ty) * 8 + tx);
  assert(IsValidSquare(move.from));
  assert(IsValidSquare(move.to));

  if text.count >= 5
  {
    if text[4] == {
      case #char "n"; promotion = .KNIGHT;
      case #char "b"; promotion = .BISHOP;
      case #char "r"; promotion = .ROOK;
      case #char "q"; promotion = .QUEEN;
    }
  }
  return move, promotion;
}

UciStringFromMove :: (move: Move) -> FixedArray(u8, 32)
{
  from := Vec2FromSquare(move.from);
  to := Vec2FromSquare(move.to);

  result: FixedArray(u8, 32);
  add(*result, xx (from.x + #char "a"));
  add(*result, xx (8 - from.y + #char "0"));
  add(*result, xx (to.x + #char "a"));
  add(*result, xx (8 - to.y + #char "0"));
  return result;
}

UciSetPosition :: (board: *Board, content: string)
{
  if StartsWith(content, "startpos")
  {
    board.* = BoardFromFen(INIT_BOARD_FEN);
  }
  else if StartsWith(content, "fen")
  {
    fen := StripWhite(Skip(content, 3));
    board.* = BoardFromFen(fen);
  }

  moves_offset, moves_found := Find(content, "moves", .FindAddNeedleCountOnSuccess);
  if moves_found
  {
    all_moves := StripWhite(Skip(content, moves_offset));
    while all_moves
    {
      move_str := Prefix(all_moves, Find(all_moves, " ", .FindAddHaystackCountOnFail));
      move, promotion := UciMoveFromString(move_str);
      MakeMove(board, move, promotion);

      all_moves = StripWhite(Skip(all_moves, move_str.count));
    }
  }
}

UciLoop :: ()
{
  rng_state = current_time_monotonic().low | 1;

  board := BoardFromFen(INIT_BOARD_FEN);
  while true
  {
    line, ok := UciReadLine();
    if !ok break;

    line = StripWhite(line);
    if !line continue;

    first := Prefix(line, Find(line, " ", .FindAddHaystackCountOnFail));
    second := StripWhite(Skip(line, first.count + 1));

    if first == {
      case "uci";
      print("id name Mati-Engine\n");
      print("id author Mateusz Garbowski\n");
      print("uciok\n");

      case "isready";
      print("readyok\n");

      case "ucinewgame";
      board = BoardFromFen(INIT_BOARD_FEN);

      case "position";
      UciSetPosition(*board, second);

      case "go";
      moves := LegalMovesFromBoard(board);
      if moves.count == 0
      {
        print("bestmove 0000\n");
      }
      else
      {
        pick := RandomNumberInRange(xx moves.count);
        print("bestmove %\n", to_string(UciStringFromMove(moves.buffer[pick])));
      }

      case "quit";
      break;
    }
  }
}
