open Base

type player = White | Black
[@@deriving compare, sexp_of]

type piece_type = Rook | Knight | Bishop | Queen | King | Pawn
[@@deriving compare, sexp_of]

type position = { row: int; column: int }
[@@deriving compare, sexp_of]

type piece = { piece_type: piece_type; owner: player }
[@@deriving compare, sexp_of]

module Piece_Position = struct 
  module T = struct
    type t = { piece: piece; position: position }
    [@@deriving compare, sexp_of]
  end
  include T
  include Comparator.Make(T)
end

type zobrist_position_keys = (Piece_Position.t, int64, Piece_Position.comparator_witness) Map.t

let all_positions =
  let open List in
  let rows = range 1 8 in
  let columns = range 1 8 in
  concat_map rows ~f:(fun row ->
    map columns ~f:(fun column -> { row; column }))

type boardState = {
  (* whitePieces: Piece list; *)
  (* blackPieces: piece list; *)
  turn: player;
  fiftyMoveCounter: int;
}
