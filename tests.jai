RunTests :: ()
{
  auto_release_temp();
  push_allocator(temp);

  OneMoveFENTests();
}

#scope_file;
OneMoveFENTests :: ()
{
  {
    Test("En passant");
    moves := TestMoves("k7/8/8/5pP1/8/8/8/1K6 w - f6 0 40");
    Check(Contains(moves, "P g5f6"));
  }

  {
    Test("Avoid check");
    moves := TestMoves("2rkr3/8/8/8/8/8/8/3K4 w - - 0 1");
    Check(moves == "K d1d2");
  }

  {
    Test("White can do both castlings");
    moves := TestMoves("r3k2r/p6p/8/8/8/8/P6P/R3K2R w KQkq - 0 1");
    Check(Contains(moves, "K e1c1"));
    Check(Contains(moves, "K e1g1"));
  }

  {
    Test("Black's queenside castling is illegal");
    moves := TestMoves("r3k2r/p6p/8/8/8/8/P6P/3RK2R b Kkq - 1 1");
    Check(Contains(moves, "k e8g8"));
    Check(Contains(moves, "k e8c8") == false); // @todo this fails
  }

  {
    Test("Black's castling would result in check");
    moves := TestMoves("r3k2r/p6p/8/8/8/8/P6P/2R1K2R b Kkq - 1 1");
    Check(Contains(moves, "k e8c8") == false);
  }

}

//
// Pretty test infrastructure
//
test_label: string;
test_fen: string;
test_moves_string: string;

test_passed: bool;
test_counter: s64;

Test :: (label: string) #expand
{
  test_label = label;
  test_passed = true;

  `defer
  {
    if test_passed log("[%] PASS: %", test_counter, test_label);
    test_label = "";
    test_counter += 1;
  }
}

TestMoves :: (fen: string) -> string
{
  test_fen = fen;
  board := BoardFromFen(fen);
  moves_array := LegalMovesFromBoard(board);
  test_moves_string = MoveArrayToString(moves_array, {board = board, pieces = .SHORT, sort = true});
  return test_moves_string;
}

Check :: (arg: Code, loc := #caller_location) #expand
{
  result: bool = #insert arg;
  if !result
  {
    #import "Print_Vars";

    test_passed = false;
    log("[%] FAIL: %", test_counter, test_label);
    log("  ARG: %", #run get_code_name(arg));
    log("  LOC: %:%:%", loc.fully_pathed_filename, loc.line_number, loc.character_number);
    log("  FEN: %", test_fen);
    log("- moves -\n%", test_moves_string);
  }
}

