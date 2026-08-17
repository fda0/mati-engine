MAJOR_VERSION :: 0;
SUBTITLE :: "Random edition";

ENGINE_NAME :: #run -> string {
  result := sprint("Mati Engine %: %%", MAJOR_VERSION, SUBTITLE, GIT_REVISION);
  log("  - - - % - - -", result);
  return result;
}

GIT_REVISION :: #run -> string {
  #import "Process";
  version: string;
  _, revision := run_command("git", "rev-parse", "--short", "HEAD", capture_and_return_output = true);
  _, status := run_command("git", "status", "--porcelain", capture_and_return_output = true);
  if revision then version = sprint(" [%%]", Trim(revision), ifx Trim(status) then "*");
  return version;
}

// Types
Piece :: enum u8
{
  PAWN;
  KNIGHT;
  BISHOP;
  ROOK;
  QUEEN;
  KING;

  INVALID;
}

PieceLetters :: "pnbrqk";

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

  WHITE_BOTH :: WHITE_KINGSIDE | WHITE_QUEENSIDE;
  BLACK_BOTH :: BLACK_KINGSIDE | BLACK_QUEENSIDE;
}

Field :: #type u64; // 8x8 bitfield - encodes occupancy for the whole board
Square :: #type s8; // [0:63] - encodes a single position on the board

Board :: struct
{
  fields: [PIECE_COUNT] Field;
  is_black: Field;

  halfmove_clock: u16;
  black_to_move: bool; // @todo look for readable tricks to reduce branching on this bool
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
PIECE_COUNT :: 6;
INVALID_SQUARE: Square : -128;

INIT_BOARD_FEN :: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

// Global variables
rng_state: u64;

// Functions
PieceFlagsFromPiece :: (piece: Piece) -> PieceFlags
{
  if piece >= .INVALID return 0;
  return xx (1 << piece);
}

SquareFromVec2 :: inline (vec: Vec2) -> Square { return xx (vec.y*8 + vec.x); }
Vec2FromSquare :: inline (s: Square) -> Vec2 { return {xx (s & 7), xx (s >> 3)}; }
IsValidSquare :: inline (s: Square) -> bool { return s < 64 && s >= 0; }
IsSameRow :: inline (from: Square, to: Square) -> bool { return ((from ^ to) & (~7)) == 0; }
ContainsSquare :: (m: Move, s: Square) -> bool { return m.from == s || m.to == s; }

FieldFromSquare :: inline (square: Square) -> Field
{
  field: Field;
  if IsValidSquare(square) field = (cast(u64)1) << square;
  return field;
}

FieldFromSquares :: inline (squares: ..Square) -> Field
{
  field: Field;
  for squares if IsValidSquare(it) field |= (cast(u64)1) << it;
  return field;
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
    if it == " " break;

    if it == #char "/"
    {
      pos.x = 0;
      pos.y += 1;
      continue;
    }
    else if IsDigit(it)
    {
      pos.x += ParseDigit(it);
      continue;
    }

    is_black := IsAlphaLower(it);
    piece := Piece.INVALID;
    for letter, letter_idx: PieceLetters
    {
      if ToLower(it) == letter
      {
        piece = xx letter_idx;
        break;
      }
    }

    if piece == .INVALID
    {
      log("Invalid fen: %", text);
      return board;
    }

    field_bit := FieldFromVec2(pos);
    board.fields[piece] |= field_bit;
    board.is_black |= ifx is_black then field_bit;

    pos.x += 1;
  }

  rest := Skip(text, processed_count);
  color := Prefix(rest, Find(rest, " "));

  rest = Skip(rest, color.count + 1);
  castling := Prefix(rest, Find(rest, " "));

  rest = Skip(rest, castling.count + 1);
  en_passant := Prefix(rest, Find(rest, " "));

  rest = Skip(rest, en_passant.count + 1);
  halfmove := Prefix(rest, Find(rest, " "));

  rest = Skip(rest, halfmove.count + 1);
  fullmove := rest;

  board.black_to_move = color == "b";
  if Contains(castling, "K") board.castling_rights |= .WHITE_KINGSIDE;
  if Contains(castling, "Q") board.castling_rights |= .WHITE_QUEENSIDE;
  if Contains(castling, "k") board.castling_rights |= .BLACK_KINGSIDE;
  if Contains(castling, "q") board.castling_rights |= .BLACK_QUEENSIDE;
  board.en_passant_square = ParseSquare(en_passant);

  return board;
}

AllMovesFromBoard :: (board: Board) -> MoveArray
{
  ROW_0 :: #run SquareFromVec2({0, 0});
  ROW_1 :: #run SquareFromVec2({0, 1});
  ROW_6 :: #run SquareFromVec2({0, 6});
  ROW_7 :: #run SquareFromVec2({0, 7});

  moves: MoveArray;
  our_color_mask := ifx board.black_to_move then board.is_black else ~board.is_black;

  our_fields: Field;
  enemy_fields: Field;
  for board.fields
  {
    our_fields |= it & our_color_mask;
    enemy_fields |= it & ~our_color_mask;
  }

  both_teams_fields := our_fields | enemy_fields;
  enemy_fields_en_passant := enemy_fields | FieldFromSquare(board.en_passant_square);

  WHITE_CASTLE_Q_MASK :: #run FieldFromSquares(ParseSquare("b1"), ParseSquare("c1"), ParseSquare("d1"));
  WHITE_CASTLE_K_MASK :: #run FieldFromSquares(ParseSquare("f1"), ParseSquare("g1"));
  BLACK_CASTLE_Q_MASK :: #run FieldFromSquares(ParseSquare("b8"), ParseSquare("c8"), ParseSquare("d8"));
  BLACK_CASTLE_K_MASK :: #run FieldFromSquares(ParseSquare("f8"), ParseSquare("g8"));

  castling_queen: Field = ~0;
  castling_king: Field = ~0;
  if board.black_to_move
  {
    if (board.castling_rights & .BLACK_QUEENSIDE) castling_queen = BLACK_CASTLE_Q_MASK;
    if (board.castling_rights & .BLACK_KINGSIDE)  castling_king  = BLACK_CASTLE_K_MASK;
  }
  else
  {
    if (board.castling_rights & .WHITE_QUEENSIDE) castling_queen = WHITE_CASTLE_Q_MASK;
    if (board.castling_rights & .WHITE_KINGSIDE)  castling_king  = WHITE_CASTLE_K_MASK;
  }

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
          continue_moving = (to_field & enemy_fields) == 0;
        }
      }
    }
    return continue_moving;
  }

  AddMove :: (from: Square, to: Square, row_check: Square = INVALID_SQUARE) -> bool #expand
  {
    return AddMoveNotInField(from, to, our_fields, row_check);
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
        dir := ifx board.black_to_move then 1 else -1;
        pawn_starting_row := ifx board.black_to_move then ROW_1 else ROW_6;
        move_count := 1 + ifx IsSameRow(current, pawn_starting_row) then 1;

        // Forward moves (no captures)
        for 1 .. move_count
        {
          if !AddMoveNotInField(current, xx (current + 8*it*dir), both_teams_fields, is_pawn = true) break;
        }

        // Diagonal moves (captures)
        AddMoveNotInField(current, xx (current + 8*dir - 1), ~enemy_fields_en_passant, xx (current + 8*dir), is_pawn = true);
        AddMoveNotInField(current, xx (current + 8*dir + 1), ~enemy_fields_en_passant, xx (current + 8*dir), is_pawn = true);
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

      if piece_flag & .KING_FLAG
      {
        if (both_teams_fields & castling_queen) == 0 then AddMove(current, current - 2);
        if (both_teams_fields & castling_king)  == 0 then AddMove(current, current + 2);
      }

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
  DEBUG :: false;

  for * moves.*
  {
    board := init_board;

    #if DEBUG log("  RIM [%] testing move %", it_index, MoveToString(it, {board=board, pieces=.SHORT}));

    castling_rook := ApplyMoveToBoard(*board, it);
    our_color_mask := board.is_black;
    if board.black_to_move then our_color_mask = ~our_color_mask;

    our_king_field := board.fields[Piece.KING] & our_color_mask;
    #if DEBUG log("bk: %", formatInt(board.fields[Piece.KING], base=2));
    #if DEBUG log("ocm: %", formatInt(our_color_mask, base=2));
    #if DEBUG log("okf.a: %", formatInt(our_king_field, base=2));
    our_king_field |= FieldFromSquare(castling_rook);
    #if DEBUG log("okf.b: %", formatInt(our_king_field, base=2));

    illegal := false;
    moves2 := AllMovesFromBoard(board);
    for moves2
    {
      #if DEBUG log("    RIM [%] against %", it_index, MoveToString(it, {board=board, pieces=.SHORT}));

      enemy_field := FieldFromSquare(it.to);
      if enemy_field & our_king_field
      {
        #if DEBUG log("    RIM [%] illegal move detected", it_index);
        // enemy captured our king or castling square
        illegal = true;
        break;
      }
    }

    if illegal then remove it;
  }
}

ApplyMoveToBoardHelper :: (board: *Board, move: Move) -> Piece
{
  from_field := FieldFromSquare(move.from);
  to_field := FieldFromSquare(move.to);

  moved_piece := Piece.INVALID;
  for * board.fields
  {
    if it.* & from_field
    {
      assert(moved_piece == .INVALID); // multiple pieces on move.from
      moved_piece = xx it_index;
    }
    it.* &= ~to_field; // remove any captured piece
  }
  assert(moved_piece != .INVALID); // no piece on move.from

  board.fields[moved_piece] &= ~from_field; // bit we moved from needs to be cleared

  promotion_flag := PieceFlagsFromPiece(move.promotion);
  if promotion_flag & (.KNIGHT_FLAG | .BISHOP_FLAG | .ROOK_FLAG | .QUEEN_FLAG)
  {
    board.fields[move.promotion] |= to_field;
  }
  else
  {
    board.fields[moved_piece] |= to_field;
  }

  board.is_black &= ~to_field;
  if from_field & board.is_black
  {
    board.is_black &= ~from_field; // lets keep empty fields white (not strictly needed)
    board.is_black |= to_field;
  }

  return moved_piece;
}

ApplyMoveToBoard :: (board: *Board, move: Move) -> castling_rook: Square
{
  moved_piece := ApplyMoveToBoardHelper(board, move);

  ROOK_WK :: #run ParseSquare("h1");
  ROOK_WQ :: #run ParseSquare("a1");
  ROOK_BK :: #run ParseSquare("h8");
  ROOK_BQ :: #run ParseSquare("a8");

  // apply castling
  castling_rook := INVALID_SQUARE;
  if moved_piece == .KING
  {
    delta_x := move.to - move.from;
    if delta_x ==  2
    {
      castling_rook = move.to - 1;
      ApplyMoveToBoardHelper(board, {from = move.from + 3, to = castling_rook});
    }
    if delta_x == -2
    {
      castling_rook := move.to + 1;
      ApplyMoveToBoardHelper(board, {from = move.from - 4, to = castling_rook});
    }
  }

  // Clear castling on king move
  if moved_piece == .KING
  {
    if board.black_to_move then board.castling_rights &= ~.BLACK_BOTH;
    else                        board.castling_rights &= ~.WHITE_BOTH;
  }

  // Clear castling on rook move / capture
  if ContainsSquare(move, ROOK_WK) board.castling_rights &= ~.WHITE_KINGSIDE;
  if ContainsSquare(move, ROOK_WQ) board.castling_rights &= ~.WHITE_QUEENSIDE;
  if ContainsSquare(move, ROOK_BK) board.castling_rights &= ~.BLACK_KINGSIDE;
  if ContainsSquare(move, ROOK_BQ) board.castling_rights &= ~.BLACK_QUEENSIDE;

  board.black_to_move = !board.black_to_move;
  return castling_rook;
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

  args := get_command_line_arguments();
  for args
  {
    if it_index == 0 continue;

    if it == "test"
    {
      RunTests();
      exit(0);
    }
  }

  UciLoop();
}

// Imports
#import "Basic";
#import "Bit_Operations";
#import "String";
#import "Sort";
#if OS == .WINDOWS {
  #import "Windows";
} else {
  #import "POSIX";
}

#load "uci.jai";
#load "chess_strings.jai";
#load "tests.jai";
#load "base_fixed_array.jai";
#load "base_string.jai";
