open Base
open ChessData

type zobrist_key =
  | Piece of player * piece_type * position
  | Castle of player
  | EnPassant of player * int
  | Turn of player
[@@deriving compare, sexp_of]

module ZobristKey = struct
  module T = struct
    type t = zobrist_key [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let all_positions : position list =
  let open List in
  let rows = range 1 8 in
  let columns = range 1 8 in
  concat_map rows ~f:(fun row -> map columns ~f:(fun column -> {row; column}))

let all_piece_types : piece_type list = [Rook; Knight; Bishop; Queen; King; Pawn]

let all_piece_keys =
  List.concat_map all_positions ~f:(fun position ->
      List.concat_map all_piece_types ~f:(fun piece_type ->
          [ Piece (White, piece_type, position)
          ; Piece (Black, piece_type, position) ] ) )

let all_castle_keys = [Castle White; Castle Black]

let all_en_passant_keys =
  List.concat_map (List.range 1 8) ~f:(fun column ->
      [EnPassant (White, column); EnPassant (Black, column)] )

let all_keys =
  List.concat
    [ all_piece_keys
    ; all_castle_keys
    ; all_en_passant_keys
    ; [Turn White; Turn Black] ]

type zobrist_dictionary =
  (zobrist_key, int64, ZobristKey.comparator_witness) Base.Map.t

let init_zobrist () =
  Map.of_alist_exn
    (module ZobristKey)
    (List.map all_keys ~f:(fun key -> (key, Random.int64 Int64.max_value)))

let toggle zobrist_dictionary state zobrist_key =
  let flag = Map.find_exn zobrist_dictionary zobrist_key in
  Int64.bit_xor state flag
