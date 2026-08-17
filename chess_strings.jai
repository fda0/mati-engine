ParseSquare :: (text: string) -> Square
{
  if text.count < 2 return INVALID_SQUARE;
  x := text[0] - #char "a";
  y := text[1] - #char "1";
  if x < 0 || x > 7 || y < 0 || y > 7 return INVALID_SQUARE;
  return ((7 - y) * 8 + x).(Square);
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
  pieces: enum { HIDE; FULL; SHORT; }
  indices: bool;
  sort: bool;
  separator := "\n";
}

append :: (builder: *String_Builder, move: Move, spec := MovePrintSpec.{})
{
  from := Vec2FromSquare(move.from);
  to := Vec2FromSquare(move.to);

  if spec.pieces != .HIDE
  {
    from_field := FieldFromSquare(move.from);
    for spec.board.fields
    {
      if it & from_field
      {
        if spec.pieces == .FULL
        {
          color := ifx from_field & spec.board.is_black then "BLACK" else "WHITE";
          print_to_builder(builder, "% % ", color, it_index.(Piece));
        }
        if spec.pieces == .SHORT
        {
          letter := PieceLetters[it_index];
          if !(from_field & spec.board.is_black) letter = ToUpper(letter);
          print_to_builder(builder, "% ", ByteStr(*letter));
        }
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

MoveToString :: (move: Move, spec := MovePrintSpec.{}) -> string
{
  builder: String_Builder;
  append(*builder, move, spec);
  return builder_to_string(*builder);
}

MoveArrayToString :: (moves_: MoveArray, spec := MovePrintSpec.{}) -> string
{
  moves := moves_;
  if spec.sort
  {
    move_compare :: (a: Move, b: Move) -> int
    {
      delta := a.from - b.from;
      if delta == 0 then a.to - b.to;
      return delta;
    }
    quick_sort(to_view(moves), move_compare);
  }

  builder: String_Builder;
  for moves
  {
    if it_index > 0 append(*builder, spec.separator);
    if spec.indices print_to_builder(*builder, "[%] ", it_index);
    append(*builder, it, spec);
  }
  return builder_to_string(*builder);
}
