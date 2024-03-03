open Base
open ChessData

let () = Random.init 123

let zobrist_dictionary = Zobrist.init_zobrist ()

let initial_board_state = InitialSetup.initial_board_state

type square_contents = SameColor | DifferentColor | Empty

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

let iterate_rider initial_position board_state offset =
  Sequence.unfold ~init:initial_position ~f:(fun current_position ->
      let row = current_position.row + offset.row_step in
      let column = current_position.column + offset.column_step in
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
    [ iterate_rider initial_position board_state {row_step= -1; column_step= 0}
    ; iterate_rider initial_position board_state {row_step= 1; column_step= 0}
    ; iterate_rider initial_position board_state {row_step= 0; column_step= -1}
    ; iterate_rider initial_position board_state {row_step= 0; column_step= 1}
    ]

let bishop_moves board_state initial_position =
  Sequence.round_robin
    [ iterate_rider initial_position board_state {row_step= -1; column_step= -1}
    ; iterate_rider initial_position board_state {row_step= -1; column_step= 1}
    ; iterate_rider initial_position board_state {row_step= 1; column_step= -1}
    ; iterate_rider initial_position board_state {row_step= 1; column_step= 1}
    ]

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

let pawn_moves board_state initial_position =
  let current_player = board_state.turn in
  let direction = match current_player with White -> 1 | Black -> -1 in
  let {row; column} = initial_position in
  let not_yet_moved =
    match current_player with White -> row = 2 | Black -> row = 7
  in
  let forward_one_target = {row; column= column + direction} in
  let forward_one =
    match Map.find board_state.pieces_by_position forward_one_target with
    | Some _ ->
        None
    | None ->
        Some (Move forward_one_target)
  in
  let forward_two_target = {row; column= column + direction + direction} in
  let forward_two =
    if not not_yet_moved then None
    else
      match Map.find board_state.pieces_by_position forward_two_target with
      | Some _ ->
          None
      | None ->
          Some (Move forward_two_target)
  in
  let capture_left_target = {row= row - 1; column= column + direction} in
  let capture_left =
    match Map.find board_state.pieces_by_position capture_left_target with
    | Some {owner; _} ->
        if compare_player owner board_state.turn = 0 then None
        else Some (Capture capture_left_target)
    | _ ->
        None
  in
  let capture_right_target = {row= row + 1; column= column + direction} in
  let capture_right =
    match Map.find board_state.pieces_by_position capture_right_target with
    | Some {owner= Black; _} ->
        Some (Capture capture_right_target)
    | _ ->
        None
  in
  let en_passant_row = match current_player with White -> 5 | Black -> 4 in
  let en_passant =
    if not (row = en_passant_row) then None
    else
      match board_state.en_passant_valid_column with
      | Some col ->
          if col - 1 = column || col + 1 = column then
            Some
              (EnPassant
                 { destination= {row= row + direction; column= col}
                 ; captured= {row; column= col} } )
          else None
      | None ->
          None
  in
  List.filter_map
    [forward_one; forward_two; capture_left; capture_right; en_passant]
    ~f:(fun x -> x )
