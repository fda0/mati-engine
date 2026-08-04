Piece :: enum u8
{
  PAWN;
  KNIGHT;
  BISHOP;
  ROOK;
  QUEEN;
  KING;
}
PIECE_COUNT :: #run enum_highest_value(Piece) + 1;

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
  pieces: [PIECE_COUNT] Field;
  is_black: Field;

  halfmove_clock: u16;
  black_to_move: u8;
  castling_rights: CastlingRights;
  en_passant_square: Square;
}

SquareFromRC :: inline (row: s64, col: s64) -> Square { return xx (row*8 + col); }
FieldFromSquare :: inline (s: Square) -> Field { return xx (1 << s); }
FieldFromRC :: inline (row: s64, col: s64) -> Field { return FieldFromSquare(SquareFromRC(row, col)); }

INIT_BOARD_FEN :: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

IsUpper :: (a: u8) -> bool { return a >= #char "A" && a <= #char "Z"; }
IsLower :: (a: u8) -> bool { return a >= #char "a" && a <= #char "z"; }
IsDigit :: (a: u8) -> bool { return a >= #char "0" && a <= #char "9"; }
ToLower :: (a: u8) -> u8 { return a - (ifx IsUpper(a) #char "a" - #char "A").(u8); }
ParseDigit :: (a: u8) -> s64 { return a - #char "0"; }

BoardFromFen :: (text: string) -> Board
{
  board: Board;
  r, c: s64;
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
      case #char "/"; c = 0; r += 1; continue;

      case;
      if IsDigit(it)
      {
        c += ParseDigit(it);
        continue;
      }
      if it == " " break;
    }

    field_bit := FieldFromRC(r, c);
    board.pieces[piece] |= field_bit;
    board.is_black |= ifx is_black then field_bit;
  }
  return board;
}

main :: ()
{
  print("hello mati\n");
}

#import "Basic";
