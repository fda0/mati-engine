RunTests :: ()
{
  auto_release_temp();
  push_allocator(temp);

  OneMoveFENTests();
}

OneMoveFENTests :: ()
{
  // En passant
  {
    board := BoardFromFen("k7/8/8/5pP1/8/8/8/1K6 w - f6 0 40");
    moves := LegalMovesFromBoard(board);
    moves_str := MoveArrayToString(moves, {board = board, pieces = .FULL, sort = true});
    print("passant:\n%", moves_str);
    assert(Contains(moves_str, "WHITE PAWN g5f6"));
  }

  AVOID_CHECK_FEN :: "2rkr3/8/8/8/8/8/8/3K4 w - - 0 1";

  // all castlings
  {
    board := BoardFromFen("r3k2r/p6p/8/8/8/8/P6P/R3K2R w KQkq - 0 1");
    moves := LegalMovesFromBoard(board);
    moves_str := MoveArrayToString(moves, {board = board, pieces = .SHORT, sort = true});
    print("all castlings:\n%", moves_str);
    assert(Contains(moves_str, "K e1c1"));
    assert(Contains(moves_str, "K e1g1"));
  }
}

