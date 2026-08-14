// Types
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

Field :: #type u64; // 8x8 bitfield - encodes occupancy for the whole board
Square :: #type s8; // [0:63] - encodes a single position on the board

Board :: struct
{
  fields: [PIECE_COUNT] Field;
  is_black: Field;

  halfmove_clock: u16;
  black_to_move: bool;
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
  promotion: Piece;
}
MoveArray :: FixedArray(Move, 256);

// Global constants
PIECE_COUNT :: #run enum_highest_value(Piece) + 1;
INVALID_SQUARE: Square : -128;

INIT_BOARD_FEN :: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

// Global variables
rng_state: u64;

// Functions
PieceFlagsFromPiece :: (piece: Piece) -> PieceFlags { return xx (1 << piece); }

SquareFromVec2 :: inline (vec: Vec2) -> Square { return xx (vec.y*8 + vec.x); }
Vec2FromSquare :: inline (s: Square) -> Vec2 { return {xx (s & 7), xx (s >> 3)}; }
IsValidSquare :: inline (s: Square) -> bool { return s < 64 && s >= 0; }
FieldFromSquare :: inline (s: Square) -> Field
{
  assert(IsValidSquare(s));
  return (cast(u64)1) << s;
}
FieldFromVec2 :: inline (vec: Vec2) -> Field { return FieldFromSquare(SquareFromVec2(vec)); }
FirstSquareInField :: inline (field: Field) -> Square { return xx (bit_scan_forward(field) - 1); }

ParseDigit :: (a: u8) -> s8 { return xx (a - #char "0"); }

BoardFromFen :: (text: string) -> Board
{
  board: Board;
  pos: Vec2;
  processed_count: s64;
  for text
  {
    defer processed_count += 1;

    is_black := IsAlphaLower(it);
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

  rest := Skip(text, processed_count);
  if StartsWith(rest, "b") board.black_to_move = true;

  return board;
}

AllMovesFromBoard :: (board: Board) -> MoveArray
{
  IsSameRow :: inline (from: Square, to: Square) -> bool
  {
    return ((from ^ to) & (~7)) == 0;
  }

  ROW_0 :: #run SquareFromVec2({0, 0});
  ROW_1 :: #run SquareFromVec2({0, 1});
  ROW_6 :: #run SquareFromVec2({0, 6});
  ROW_7 :: #run SquareFromVec2({0, 7});

  moves: MoveArray;
  our_color_mask := ifx board.black_to_move then board.is_black else ~board.is_black;

  our_field_sum: Field;
  enemy_field_sum: Field;
  for board.fields
  {
    our_field_sum |= it & our_color_mask;
    enemy_field_sum |= it & ~our_color_mask;
  }
  both_teams_field_sum := our_field_sum | enemy_field_sum;

  AddMoveNotInField :: (from: Square, to: Square, exclude_field: Field,
                        row_check: Square = INVALID_SQUARE, is_pawn := false) -> bool #expand
  {
    promotion := Piece.PAWN;
    continue_moving := false;
    if IsValidSquare(to)
    {
      if row_check == INVALID_SQUARE || IsSameRow(row_check, to)
      {
        to_field := FieldFromSquare(to);
        if (to_field & exclude_field) == 0
        {
          promotions: [] Piece = .[.PAWN];
          if is_pawn && (IsSameRow(to, ROW_0) || IsSameRow(to, ROW_7))
          {
            promotions = .[.KNIGHT, .BISHOP, .ROOK, .QUEEN];
          }

          for promotions add(*moves, {from, to, it});
          continue_moving = (to_field & enemy_field_sum) == 0;
        }
      }
    }
    return continue_moving;
  }

  AddMove :: (from: Square, to: Square, row_check: Square = INVALID_SQUARE) -> bool #expand
  {
    return AddMoveNotInField(from, to, our_field_sum, row_check);
  }

  for field, piece_idx: board.fields
  {
    piece_flag := PieceFlagsFromPiece(xx piece_idx);
    our_field := field & our_color_mask;

    our_filed_left_to_check := our_field;
    while our_filed_left_to_check
    {
      current := FirstSquareInField(our_filed_left_to_check);
      our_filed_left_to_check &= ~FieldFromSquare(current);

      // PAWN moves
      if piece_flag & .PAWN_FLAG
      {
        direction := ifx board.black_to_move then 1 else -1;
        pawn_starting_row := ifx board.black_to_move then ROW_1 else ROW_6;
        move_count := 1 + ifx IsSameRow(current, pawn_starting_row) then 1;

        // Forward moves (no captures)
        for 1 .. move_count
        {
          if !AddMoveNotInField(current, xx (current + 8*it*direction), both_teams_field_sum, is_pawn = true) break;
        }

        // Diagonal moves (captures)
        AddMoveNotInField(current, xx (current + 8*direction - 1), ~enemy_field_sum, xx (current + 8*direction), is_pawn = true);
        AddMoveNotInField(current, xx (current + 8*direction + 1), ~enemy_field_sum, xx (current + 8*direction), is_pawn = true);
      }

      // Side moves (ROOK, QUEEN, KING)
      move_side := 0;
      if piece_flag & (.ROOK_FLAG | .QUEEN_FLAG) then move_side = 7;
      if piece_flag & .KING_FLAG then move_side = 1;
      for 1 .. move_side if !AddMove(current, xx (current - 8*it)) break; // up
      for 1 .. move_side if !AddMove(current, xx (current + 8*it)) break; // down
      for 1 .. move_side if !AddMove(current, xx (current - 1*it), current) break; // left
      for 1 .. move_side if !AddMove(current, xx (current + 1*it), current) break; // right

      // Diagonal moves (.BISHOP, QUEEN, KING)
      diagonal_moves := 0;
      if piece_flag & (.BISHOP_FLAG | .QUEEN_FLAG) then diagonal_moves = 7;
      if piece_flag & (.KING_FLAG) then diagonal_moves = 1;
      for 1 .. diagonal_moves if !AddMove(current, xx (current - 9*it), xx(current - 8*it)) break;
      for 1 .. diagonal_moves if !AddMove(current, xx (current - 7*it), xx(current - 8*it)) break;
      for 1 .. diagonal_moves if !AddMove(current, xx (current + 7*it), xx(current + 8*it)) break;
      for 1 .. diagonal_moves if !AddMove(current, xx (current + 9*it), xx(current + 8*it)) break;

      // Knight
      if piece_flag & .KNIGHT_FLAG
      {
        AddMove(current, current - 16 - 1, current-16);
        AddMove(current, current - 16 + 1, current-16);
        AddMove(current, current + 16 + 1, current+16);
        AddMove(current, current + 16 - 1, current+16);
        AddMove(current, current - 8 - 2, current-8);
        AddMove(current, current - 8 + 2, current-8);
        AddMove(current, current + 8 - 2, current+8);
        AddMove(current, current + 8 + 2, current+8);
      }
    }
  }

  return moves;
}

LegalMovesFromBoard :: (board: Board) -> MoveArray
{
  moves := AllMovesFromBoard(board);
  RemoveIllegalMoves(board, *moves); // @todo LOOK ONE MOVE AHEAD!!!
  return moves;
}

RemoveIllegalMoves :: (init_board: Board, moves: *MoveArray)
{
  for * moves.*
  {
    board := init_board;
    print("[%] In RemoveIllegalMoves Loop 1, move: ", it_index); PrettyPrintMove(board, it);
    ApplyMoveToBoard(*board, it);

    king_count := popcount(board.fields[Piece.KING]);
    assert(king_count == 2);


    moves2 := AllMovesFromBoard(board);
    for moves2
    {
      print("  [%] In RemoveIllegalMoves Loop 2, move: ", it_index); PrettyPrintMove(board, it);

      board2 := board;
      ApplyMoveToBoard(*board2, it);
      king_count = popcount(board2.fields[Piece.KING]);

      if king_count < 2
      {
        log("    !!!  king count: %", king_count);
        break;
      }
    }

    // if king_count == 1 then it's a pat? IDK? it's legal i guess?
    if king_count < 2 then remove it;
  }
}

ApplyMoveToBoard :: (board: *Board, move: Move)
{
  from_field := FieldFromSquare(move.from);
  to_field := FieldFromSquare(move.to);

  moving_piece_index := -1;
  for * board.fields
  {
    if it.* & from_field
    {
      assert(moving_piece_index < 0); // multiple pieces on move.from
      moving_piece_index = it_index;
    }
    it.* &= ~to_field; // remove any captured piece
  }
  assert(moving_piece_index >= 0); // no piece on move.from

  board.fields[moving_piece_index] &= ~from_field;

  promotion_flag := PieceFlagsFromPiece(move.promotion);
  if promotion_flag & (.KNIGHT_FLAG | .BISHOP_FLAG | .ROOK_FLAG | .QUEEN_FLAG)
  {
    board.fields[move.promotion] |= to_field;
  }
  else
  {
    board.fields[moving_piece_index] |= to_field;
  }

  board.is_black &= ~to_field;
  if from_field & board.is_black
  {
    board.is_black &= ~from_field;
    board.is_black |= to_field;
  }

  board.black_to_move = !board.black_to_move;
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

FullRandomNumber :: () -> u64
{
  rng_state ^= rng_state << 13;
  rng_state ^= rng_state >> 7;
  rng_state ^= rng_state << 17;
  return rng_state;
}

RandomNumberInRange :: (max: u64) -> u64
{
  return FullRandomNumber() % max;
}

main :: ()
{
  rng_state = current_time_monotonic().low | 1;
  rng_state = 1129873231; // init rng state for debugging

  test_init_moves: bool;

  args := get_command_line_arguments();
  for args
  {
    if it_index == 0 continue;

    if it == "test" test_init_moves = true;
  }

  if test_init_moves
  {
    AVOID_CHECK_FEN :: "2rkr3/8/8/8/8/8/8/3K4 w - - 0 1";
    TEST_FEN :: "2b1n1k1/nr6/8/N1pp3R/3B1pB1/1p4p1/5p2/3K4 b - - 5 62";
    // board := BoardFromFen(INIT_BOARD_FEN);
    board := BoardFromFen(TEST_FEN);

    moves := LegalMovesFromBoard(board);
    for moves
    {
      print("[move %] ", it_index);
      PrettyPrintMove(board, it);
    }
  }
  else
  {
    UciLoop();
  }
}

// Imports
#import "Basic";
#import "Bit_Operations";
#import "String";
#if OS == .WINDOWS {
  #import "Windows";
} else {
  #import "POSIX";
}

#load "uci.jai";
#load "base_fixed_array.jai";
#load "base_string.jai";
