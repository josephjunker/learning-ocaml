open Base
open ChessData

let position_exists position =
  position.row > 0 && position.row < 9 && position.column > 0
  && position.column < 9

let get_possibly_capturing_target board_state position =
  if not (position_exists position) then None
  else
    let occupying_piece = Map.find board_state.pieces_by_position position in
    match occupying_piece with
    | None ->
        Some (Move position)
    | Some {owner; _} ->
        if compare_player board_state.turn owner = 0 then None
        else Some (Capture position)

type rider_offset = {row_step: int; column_step: int}

let iterate_rider initial_position board_state ~row_step ~column_step =
  Sequence.unfold ~init:initial_position ~f:(fun current_position ->
      let row = current_position.row + row_step in
      let column = current_position.column + column_step in
      let new_position = {row; column} in
      let target_move =
        get_possibly_capturing_target board_state new_position
      in
      match target_move with
      | Some move ->
          Some (move, new_position)
      | None ->
          None )

let rook_moves board_state initial_position =
  Sequence.round_robin
    [ iterate_rider initial_position board_state ~row_step:(-1) ~column_step:0
    ; iterate_rider initial_position board_state ~row_step:1 ~column_step:0
    ; iterate_rider initial_position board_state ~row_step:0 ~column_step:(-1)
    ; iterate_rider initial_position board_state ~row_step:0 ~column_step:1 ]

let bishop_moves board_state initial_position =
  Sequence.round_robin
    [ iterate_rider initial_position board_state ~row_step:(-1) ~column_step:(-1)
    ; iterate_rider initial_position board_state ~row_step:(-1) ~column_step:1
    ; iterate_rider initial_position board_state ~row_step:1 ~column_step:(-1)
    ; iterate_rider initial_position board_state ~row_step:1 ~column_step:1 ]

let queen_moves board_state initial_position =
  Sequence.round_robin
    [ rook_moves board_state initial_position
    ; bishop_moves board_state initial_position ]

let knight_moves board_state {row; column} =
  List.filter_map
    [ {row= row - 2; column= column - 1}
    ; {row= row - 2; column= column + 1}
    ; {row= row - 1; column= column + 2}
    ; {row= row + 1; column= column + 2}
    ; {row= row + 2; column= column + 1}
    ; {row= row + 2; column= column - 1}
    ; {row= row + 1; column= column - 2}
    ; {row= row - 1; column= column - 2} ]
    ~f:(fun new_position ->
      get_possibly_capturing_target board_state new_position )

let king_moves board_state {row; column} =
  List.filter_map
    [ {row= row - 1; column}
    ; {row= row - 1; column= column + 1}
    ; {row; column= column + 1}
    ; {row= row + 1; column= column + 1}
    ; {row= row + 1; column}
    ; {row= row + 1; column= column - 1}
    ; {row; column= column - 1}
    ; {row= row - 1; column= column - 1} ]
    ~f:(fun new_position ->
      get_possibly_capturing_target board_state new_position )

type pawn_rules =
  { direction: int
  ; current_player: player
  ; opponent: player
  ; promotion_row: int
  ; unmoved_row: int
  ; en_passant_row: int }

let white_pawn_rules =
  { direction= 1
  ; current_player= White
  ; opponent= Black
  ; promotion_row= 8
  ; unmoved_row= 2
  ; en_passant_row= 5 }

let black_pawn_rules =
  { direction= -1
  ; current_player= Black
  ; opponent= White
  ; promotion_row= 1
  ; unmoved_row= 7
  ; en_passant_row= 4 }

let promote_pawn position =
  [Promotion (Queen, position); Promotion (Knight, position)]

let capture_promote_pawn position =
  [CapturePromotion (Queen, position); CapturePromotion (Knight, position)]

let pawn_forward_two initial_position direction board_state =
  let {row; column} = initial_position in
  let forward_two_target = {row= row + direction + direction; column} in
  match Map.find board_state.pieces_by_position forward_two_target with
  | Some _ ->
      []
  | None ->
      [Move forward_two_target]

let pawn_forward pawn_rules initial_position board_state =
  let {row; column} = initial_position in
  let {direction; unmoved_row; promotion_row; _} = pawn_rules in
  let forward_one_target = {row= row + direction; column} in
  match Map.find board_state.pieces_by_position forward_one_target with
  | Some _ ->
      []
  | None ->
      if row + direction = promotion_row then promote_pawn forward_one_target
      else if row = unmoved_row then
        List.concat
          [ pawn_forward_two initial_position direction board_state
          ; [Move forward_one_target] ]
      else [Move forward_one_target]

let pawn_capture pawn_rules column_direction initial_position board_state =
  let {direction; opponent; promotion_row; _} = pawn_rules in
  let {row; column} = initial_position in
  let capture_target =
    {row= row + direction; column= column + column_direction}
  in
  match Map.find board_state.pieces_by_position capture_target with
  | Some other_piece ->
      if compare_player other_piece.owner opponent = 0 then []
      else if capture_target.row = promotion_row then
        capture_promote_pawn capture_target
      else [Capture capture_target]
  | _ ->
      []

let pawn_capture_left pawn_rules initial_position board_state =
  pawn_capture pawn_rules (-1) initial_position board_state

let pawn_capture_right pawn_rules initial_position board_state =
  pawn_capture pawn_rules 1 initial_position board_state

let en_passant pawn_rules initial_position board_state =
  let {row; column} = initial_position in
  let {direction; en_passant_row; _} = pawn_rules in
  if not (row = en_passant_row) then []
  else
    match board_state.en_passant_valid_column with
    | Some col ->
        if col - 1 = column || col + 1 = column then
          [ EnPassant
              { destination= {row= row + direction; column= col}
              ; captured= {row; column= col} } ]
        else []
    | None ->
        []

let pawn_moves pawn_rules board_state initial_position =
  List.concat
    [ pawn_forward pawn_rules initial_position board_state
    ; pawn_capture_left pawn_rules initial_position board_state
    ; pawn_capture_right pawn_rules initial_position board_state
    ; en_passant pawn_rules initial_position board_state ]
