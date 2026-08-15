ParseSquare :: (text: string, validate := false) -> Square
{
  if validate
  {
    if text.count < 2 || !IsAlpha(text[0]) || !IsDigit(text[1])
    {
      return INVALID_SQUARE;
    }
  }

  assert(text.count >= 2);
  x := text[0] - #char "a";
  y := text[1] - #char "1";
  result := ((7 - y) * 8 + x).(Square);
  assert(IsValidSquare(result));
  return result;
}

ParseMove :: (text: string) -> Move
{
  move := Move.{
    from = ParseSquare(text),
    to = ParseSquare(Skip(text, 2))
  };

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

MovePrintSpec :: struct
{
  board: Board;
  piece: bool;
  indices: bool;
}

append :: (builder: *String_Builder, move: Move, spec := MovePrintSpec.{})
{
  from := Vec2FromSquare(move.from);
  to := Vec2FromSquare(move.to);

  if spec.piece
  {
    from_field := FieldFromSquare(move.from);

    for spec.board.fields
    {
      if it & from_field
      {
        color := ifx from_field & spec.board.is_black then "Black" else "White";
        print_to_builder(builder, "% % ", color, it_index.(Piece));
      }
    }
  }

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

MoveArrayToString :: (moves: MoveArray, spec := MovePrintSpec.{}) -> string
{
  builder: String_Builder;
  for moves
  {
    if spec.indices print_to_builder(*builder, "[%] ", it_index);
    append(*builder, it, spec);
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
