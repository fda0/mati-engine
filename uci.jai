uci_rng_state: u64;

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

UciMoveFromString :: (text: string) -> Move, Piece, bool
{
  move: Move;
  promotion: Piece;
  if text.count < 4 return move, promotion, false;

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
  return move, promotion, true;
}

UciStringFromMove :: (move: Move) -> string
{
  from := Vec2FromSquare(move.from);
  to := Vec2FromSquare(move.to);

  chars: [4] u8;
  chars[0] = (from.x + #char "a").(u8);
  chars[1] = (8 - from.y + #char "0").(u8);
  chars[2] = (to.x + #char "a").(u8);
  chars[3] = (8 - to.y + #char "0").(u8);
  return copy_string(string.{count = 4, data = chars.data});
}

UciSetPosition :: (board: *Board, parts: [] string)
{
  moves_index := parts.count;
  for parts if it == "moves" { moves_index = it_index; break; }

  if parts.count > 1 && parts[1] == "startpos"
  {
    board.* = BoardFromFen(INIT_BOARD_FEN);
  }
  else if parts.count > 2 && parts[1] == "fen"
  {
    board.* = BoardFromFen(parts[2]);
    if min(moves_index, parts.count) > 3
      board.black_to_move = (parts[3] == "b");
  }

  for moves_index + 1 .. parts.count - 1
  {
    move, promotion, ok := UciMoveFromString(parts[it]);
    if ok MakeMove(board, move, promotion);
  }
}

UciLoop :: ()
{
  uci_rng_state = current_time_monotonic().low | 1;

  board := BoardFromFen(INIT_BOARD_FEN);
  while true
  {
    line, ok := UciReadLine();
    if !ok break;

    parts := split(trim(line), " ");
    if parts.count == 0 continue;

    if parts[0] == {
      case "uci";
        print("id name mati\n");
        print("id author Mateusz Garbowski\n");
        print("uciok\n");
      case "isready";
        print("readyok\n");
      case "ucinewgame";
        board = BoardFromFen(INIT_BOARD_FEN);
      case "position";
        UciSetPosition(*board, parts);
      case "go";
        moves := LegalMovesFromBoard(board);
        if moves.count == 0
        {
          print("bestmove 0000\n");
        }
        else
        {
          pick := RandomNumberInRange(xx moves.count);
          print("bestmove %\n", UciStringFromMove(moves.buffer[pick]));
        }
      case "quit";
        break;
    }
  }
}
