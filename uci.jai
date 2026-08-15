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
      move  := ParseMove(move_str);
      ApplyMoveToBoard(board, move);

      all_moves = StripWhite(Skip(all_moves, move_str.count));
    }
  }
}

UciLoop :: ()
{
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
        builder: String_Builder;
        append(*builder, "bestmove ");
        append(*builder, moves[pick]);
        append(*builder, "\n");
        write_builder(*builder);
      }

      case "quit";
      break;
    }
  }
}
