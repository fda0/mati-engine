Piece :: enum u8
{
  PAWN;
  KNIGHT;
  BISHOP;
  ROOK;
  QUEEN;
  KING;
}

PieceFlags :: enum_flags u8
{
  PAWN_FLAG;
  KNIGHT_FLAG;
  BISHOP_FLAG;
  ROOK_FLAG;
  QUEEN_FLAG;
  KING_FLAG;
}

CastlingRights :: enum_flags u8
{
  WHITE_KINGSIDE;
  WHITE_QUEENSIDE;
  BLACK_KINGSIDE;
  BLACK_QUEENSIDE;
}

Field :: #type u64;
Square :: #type u8;

Board :: struct
{
  fields: [PIECE_COUNT] Field;
  is_black: Field;

  halfmove_clock: u16;
  black_to_move: u8;
  castling_rights: CastlingRights;
  en_passant_square: Square;
}

Vec2 :: struct
{
  x, y: s8;
}

Move :: struct
{
  from: Square;
  to: Square;
}
MoveArray :: struct
{
  buffer: [MAX_LEGAL_MOVES] Move;
  count: s64;
}

PIECE_COUNT :: #run enum_highest_value(Piece) + 1;
MAX_LEGAL_MOVES :: 256;

INIT_BOARD_FEN :: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

SquareFromVec2 :: inline (vec: Vec2) -> Square { return xx (vec.y*8 + vec.x); }
Vec2FromSquare :: inline (s: Square) -> Vec2 { return {xx (s & 7), xx (s >> 3)}; }
FieldFromSquare :: inline (s: Square) -> Field { return (cast(u64)1) << s; }
FieldFromVec2 :: inline (vec: Vec2) -> Field { return FieldFromSquare(SquareFromVec2(vec)); }
FirstSquareInField :: inline (field: Field) -> Square { return xx (bit_scan_forward(field) - 1); }

IsUpper :: (a: u8) -> bool { return a >= #char "A" && a <= #char "Z"; }
IsLower :: (a: u8) -> bool { return a >= #char "a" && a <= #char "z"; }
IsDigit :: (a: u8) -> bool { return a >= #char "0" && a <= #char "9"; }
ToLower :: (a: u8) -> u8 { return a + (ifx IsUpper(a) #char "a" - #char "A").(u8); }
ParseDigit :: (a: u8) -> s8 { return xx (a - #char "0"); }

BoardFromFen :: (text: string) -> Board
{
  board: Board;
  pos: Vec2;
  for text
  {
    is_black := IsLower(it);
    piece: Piece;
    if ToLower(it) == {
      case #char "p"; piece = .PAWN;
      case #char "n"; piece = .KNIGHT;
      case #char "b"; piece = .BISHOP;
      case #char "r"; piece = .ROOK;
      case #char "q"; piece = .QUEEN;
      case #char "k"; piece = .KING;
      case #char "/"; pos.x = 0; pos.y += 1; continue;

      case;
      if IsDigit(it)
      {
        pos.x += ParseDigit(it);
        continue;
      }
      if it == " " break;
    }

    field_bit := FieldFromVec2(pos);
    board.fields[piece] |= field_bit;
    board.is_black |= ifx is_black then field_bit;

    pos.x += 1;
  }
  return board;
}

LegalMovesFromBoard :: (board: Board) -> MoveArray
{
  UNDER_4TH_BIT_MASK :: (7).(u64);
  ABOVE_3RD_BIT_MASK :: (~7).(u64);

  DoesNotChangeRow :: inline (from: Square, to: Square) -> bool
  {
    return ((from ^ to) & ABOVE_3RD_BIT_MASK) == 0;
  }
  DoesNotChangeCol :: inline (from: Square, to: Square) -> bool
  {
    return ((from ^ to) & UNDER_4TH_BIT_MASK) == 0;
  }

  moves: MoveArray;
  AddMove :: (from: Square, to: Square) #expand
  {
    moves.buffer[moves.count] = {from, to};
    moves.count += 1;
  }

  our_color_mask := ifx board.black_to_move then board.is_black else ~board.is_black;

  our_field_sum: Field;
  enemy_field_sum: Field;
  for board.fields
  {
    our_field_sum |= it & our_color_mask;
    enemy_field_sum |= it & ~our_color_mask;
  }

  TestAddMove :: (from: Square, to: Square) #expand
  {
    if to != 0
    {
      to_field := FieldFromSquare(to);
      if (to_field & our_field_sum) == 0
      {
        AddMove(from, to);
      }
    }
  }

  for piece_field_both_sides, piece_idx: board.fields
  {
    piece_flag := (1 << piece_idx).(PieceFlags);
    our_piece_field := piece_field_both_sides & our_color_mask;

    to_check_field := our_piece_field;
    while to_check_field
    {
      current_square := FirstSquareInField(to_check_field);
      to_check_field &= ~((cast(u64)1) << current_square);

       // @todo think about: the loop could operate on fields instead of squares

      // Move down
      if piece_flag & (.PAWN_FLAG | .ROOK_FLAG | .QUEEN_FLAG | .KING_FLAG)
      {
        candidate_square := current_square + 8;
        if DoesNotChangeCol(current_square, candidate_square)
        {
          TestAddMove(current_square, candidate_square);
        }
      }

      // Move up
      if piece_flag & (.PAWN_FLAG | .ROOK_FLAG | .QUEEN_FLAG | .KING_FLAG)
      {
        candidate_square := current_square - 8;
        if DoesNotChangeCol(current_square, candidate_square)
        {
          TestAddMove(current_square, candidate_square);
        }
      }

      // Move left
      if piece_flag & (.ROOK_FLAG | .QUEEN_FLAG | .KING_FLAG)
      {
        candidate_square := current_square - 1;
        if DoesNotChangeRow(current_square, candidate_square)
        {
          TestAddMove(current_square, candidate_square);
        }
      }

      // Move right
      if piece_flag & (.ROOK_FLAG | .QUEEN_FLAG | .KING_FLAG)
      {
        candidate_square := current_square + 1;
        if DoesNotChangeRow(current_square, candidate_square)
        {
          TestAddMove(current_square, candidate_square);
        }
      }

      // if piece_idx.(Piece) == {
      //   case .PAWN;
      //   case .KNIGHT;
      //   case .BISHOP;
      //   case .ROOK;
      //   case .QUEEN;
      //   case .KING;
      // }
    }
  }

  return moves;
}

PrettyPrintVec2 :: (vec: Vec2)
{
  char := (vec.x + #char "A").(u8);
  str := string.{data = *char, count = 1};
  print("%%", str, 8 - vec.y);
  print(" (raw %)", vec);
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

main :: ()
{
  print("hello mati\n");
  board := BoardFromFen(INIT_BOARD_FEN);

  moves := LegalMovesFromBoard(board);
  for 0 .. moves.count-1
  {
    print("[move %] ", it);
    PrettyPrintMove(board, moves.buffer[it]);
  }
}

#import "Basic";
#import "Bit_Operations";
