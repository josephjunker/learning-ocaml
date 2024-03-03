open Base

type player = White | Black [@@deriving compare, sexp_of]

type piece_type = Rook | Knight | Bishop | Queen | King | Pawn
[@@deriving compare, sexp_of]

type position = {row: int; column: int} [@@deriving compare, sexp_of]

type piece = {piece_type: piece_type; owner: player}
[@@deriving compare, sexp_of]

module Piece_Position = struct
  module T = struct
    type t = {piece: piece; position: position} [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module Position = struct
  module T = struct
    type t = position [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

type pieces_by_position = (Position.t, piece, Position.comparator_witness) Map.t

type piece_set = (Piece_Position.t, Piece_Position.comparator_witness) Set.t

type board_state =
  { turn: player
  ; pieces_by_position: pieces_by_position
  ; white_pieces: piece_set
  ; black_pieces: piece_set
  ; white_can_castle: bool
  ; black_can_castle: bool
  ; en_passant_valid_column: int option
  ; fifty_move_counter: int
  ; zobrist_history: int64 list }

type move =
  | Move of position
  | Capture of position
  | EnPassant of {destination: position; captured: position}
  | Promotion of piece_type * position
  | CapturePromotion of piece_type * position
