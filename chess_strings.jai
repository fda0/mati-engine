ParseMove :: (text: string) -> Move
{
  move: Move;
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
      case #char "n"; move.promotion = .KNIGHT;
      case #char "b"; move.promotion = .BISHOP;
      case #char "r"; move.promotion = .ROOK;
      case #char "q"; move.promotion = .QUEEN;
    }
  }
  return move;
}

append :: (builder: *String_Builder, move: Move, verbose := false)
{
  from := Vec2FromSquare(move.from);
  to := Vec2FromSquare(move.to);



  append(builder, (#char "a" + from.x).(u8));
  append(builder, (#char "0" + 8 - from.y).(u8));
  append(builder, (#char "a" + to.x).(u8));
  append(builder, (#char "0" + 8 - to.y).(u8));
  if move.promotion == {
    case .KNIGHT; append(builder, #char "n");
    case .BISHOP; append(builder, #char "b");
    case .ROOK;   append(builder, #char "r");
    case .QUEEN;  append(builder, #char "q");
  }
}

MoveArrayToString :: (moves: MoveArray, indices := true) -> string
{
  builder: String_Builder;
  for moves
  {
    if indices print_to_builder(*builder, "[%] ", it_index);
    append(*builder, it);
    append(*builder, "\n");
  }
  return builder_to_string(*builder);
}

PrettyPrintVec2 :: (vec: Vec2)
{
  char := (vec.x + #char "A").(u8);
  str := string.{data = *char, count = 1};
  print("%%", str, 8 - vec.y);
}

PrettyPrintMove :: (board: Board, move: Move)
{
  from_field := FieldFromSquare(move.from);
  for board.fields
  {
    if it & from_field
    {
      color := ifx from_field & board.is_black then "Black" else "White";
      print("% % ", color, it_index.(Piece));
    }
  }

  from_pos := Vec2FromSquare(move.from);
  to_pos := Vec2FromSquare(move.to);
  PrettyPrintVec2(from_pos);
  print(" -> ");
  PrettyPrintVec2(to_pos);
  print("\n");
}
