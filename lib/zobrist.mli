type zobrist_key =
  | Piece of ChessData.player * ChessData.piece_type * ChessData.position
  | Castle of ChessData.player
  | EnPassant of ChessData.player * int
  | Turn of ChessData.player

type zobrist_dictionary

val init_zobrist : unit -> zobrist_dictionary

val toggle : zobrist_dictionary -> int64 -> zobrist_key -> int64
