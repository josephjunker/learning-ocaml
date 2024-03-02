open Base
open ChessData

let white_initial_pieces = [
  (Rook, 1, 1);
  (Knight, 1, 2);
  (Bishop, 1, 3);
  (Queen, 1, 4);
  (King, 1, 5);
  (Bishop, 1, 6);
  (Knight, 1, 7);
  (Rook, 1, 8);
  (Pawn, 2, 1);
  (Pawn, 2, 2);
  (Pawn, 2, 3);
  (Pawn, 2, 4);
  (Pawn, 2, 5);
  (Pawn, 2, 6);
  (Pawn, 2, 7);
  (Pawn, 2, 8);
]

let black_initial_pieces = [
  (Rook, 8, 1);
  (Knight, 8, 2);
  (Bishop, 8, 3);
  (Queen, 8, 4);
  (King, 8, 5);
  (Bishop, 8, 6);
  (Knight, 8, 7);
  (Rook, 8, 8);
  (Pawn, 7, 1);
  (Pawn, 7, 2);
  (Pawn, 7, 3);
  (Pawn, 7, 4);
  (Pawn, 7, 5);
  (Pawn, 7, 6);
  (Pawn, 7, 7);
  (Pawn, 7, 8);
]

let add_piece_positions_to_map map piece_locations player =
  List.fold piece_locations ~init:map ~f:(fun map (piece_type, row, column) ->
    let position = { row; column } in
    let contents = { piece_type; owner = player } in
    Map.set map ~key:position ~data:contents)

let add_piece_to_set set piece_locations player =
  List.fold piece_locations ~init:set ~f:(fun set (piece_type, row, column) ->
    let open Piece_Position in
    let position = { row; column } in
    let piece = { piece_type; owner = player } in
    Set.add set { piece; position })

let initial_pieces_by_position =
  let white_pieces = add_piece_positions_to_map (Map.empty (module Position)) white_initial_pieces White in
  add_piece_positions_to_map white_pieces black_initial_pieces Black

let initial_white_pieces = add_piece_to_set (Set.empty (module Piece_Position)) white_initial_pieces White
let initial_black_pieces = add_piece_to_set (Set.empty (module Piece_Position)) black_initial_pieces Black

let initial_board_state: board_state = {
  turn = White;
  pieces_by_position = initial_pieces_by_position;
  white_pieces = initial_white_pieces;
  black_pieces = initial_black_pieces;

  white_can_castle = true;
  black_can_castle = true;

  en_passant_valid_column = None;

  fifty_move_counter = 0;
  zobrist_history = [];
}
