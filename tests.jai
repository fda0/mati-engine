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
    moves_str := MoveArrayToString(moves, {board=board, piece=true, sort = true});
    print("%", moves_str);
    assert(Contains(moves_str, "White PAWN g5f6"));
  }

  AVOID_CHECK_FEN :: "2rkr3/8/8/8/8/8/8/3K4 w - - 0 1";
}

