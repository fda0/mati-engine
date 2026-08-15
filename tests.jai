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
    board := BoardFromFen("rnbqkbnr/ppp2ppp/4p3/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3");
    moves := LegalMovesFromBoard(board);
    moves_str := MoveArrayToString(moves);
    log("en_passant moves:\n%", moves_str);
  }

  AVOID_CHECK_FEN :: "2rkr3/8/8/8/8/8/8/3K4 w - - 0 1";
}

