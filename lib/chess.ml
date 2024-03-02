open Base
open ChessData

let () = Random.init 123

let zobrist_dictionary = Zobrist.init_zobrist()
let initial_board_state = InitialSetup.initial_board_state

let is_valid_target board_state position =
  (position.row > 0) && (position.row < 9) && (position.column > 0) && (position.column < 9) && (
    let occupying_piece = Map.find board_state.pieces_by_position position in
    match occupying_piece with
    | None -> true
    | Some { owner; _ } -> compare_player board_state.turn owner <> 0
  )

type stride_offset = { row_step: int; column_step: int }
let iterate_stride initial_position board_state stride_offset =
  Sequence.unfold ~init:initial_position ~f:(fun current_position ->
    let row = current_position.row + stride_offset.row_step in
    let column = current_position.column + stride_offset.column_step in
    let new_position = { row; column } in
    let is_valid = is_valid_target board_state new_position in
    match is_valid with
    | true -> Some (new_position, new_position)
    | false -> None)

let rook_moves = fun board_state initial_position ->
  Sequence.round_robin [
    iterate_stride initial_position board_state { row_step = -1; column_step = 0 };
    iterate_stride initial_position board_state { row_step = +1; column_step = 0 };
    iterate_stride initial_position board_state { row_step = 0; column_step = -1 };
    iterate_stride initial_position board_state { row_step = 0; column_step = +1 };
  ]

let bishop_moves = fun board_state initial_position ->
  Sequence.round_robin [
    iterate_stride initial_position board_state { row_step = -1; column_step = -1 };
    iterate_stride initial_position board_state { row_step = -1; column_step = 1 };
    iterate_stride initial_position board_state { row_step = 1; column_step = -1 };
    iterate_stride initial_position board_state { row_step = 1; column_step = 1 };
  ]

let queen_moves = fun board_state initial_position ->
  Sequence.round_robin [
    rook_moves board_state initial_position;
    bishop_moves board_state initial_position;
  ]

let knight_moves = fun board_state { row; column } ->
  List.filter [
    { row = row - 2; column = column - 1};
    { row = row - 2; column = column + 1};
    { row = row - 1; column = column + 2};
    { row = row + 1; column = column + 2};
    { row = row + 2; column = column + 1};
    { row = row + 2; column = column - 1};
    { row = row + 1; column = column - 2};
    { row = row - 1; column = column - 2};
  ] ~f:(fun new_position -> is_valid_target board_state new_position)

let king_moves = fun board_state { row; column } ->
  List.filter [
    { row = row - 1; column };
    { row = row - 1; column = column + 1};
    { row = row; column = column + 1};
    { row = row + 1; column = column + 1};
    { row = row + 1; column = column };
    { row = row + 1; column = column - 1 };
    { row = row; column = column - 1};
    { row = row - 1; column = column - 1};
  ] ~f:(fun new_position -> is_valid_target board_state new_position)

let pawn_moves board_state initial_position =
  let not_yet_moved = match board_state.turn with
  | White -> initial_position.row = 2
  | Black -> initial_position.row = 7
in not_yet_moved

let white_pawn_moves board_state initial_position not_yet_moved =
  let { row; column } = initial_position in
  let forward_one_target = { row ; column = column + 1 } in
  let forward_one = match Map.find board_state.pieces_by_position forward_one_target with
  | Some (_) -> None
  | None -> Some forward_one_target in
  let forward_two_target = { row;  column = column + 2 } in
  let forward_two = if not not_yet_moved then None else match Map.find board_state.pieces_by_position forward_two_target with
  | Some (_) -> None
  | None -> Some forward_two_target in
  let capture_left_target = { row = row - 1; column = column + 1 } in
  let capture_left = match Map.find board_state.pieces_by_position capture_left_target with
  | Some ({ owner = Black; _}) -> Some capture_left_target
  | _ -> None in
  let capture_right_target = { row = row + 1; column = column + 1 } in
  let capture_right = match Map.find board_state.pieces_by_position capture_right_target with
  | Some ({ owner = Black; _}) -> Some capture_right_target
  | _ -> None in
  List.filter_map [forward_one; forward_two; capture_left; capture_right] ~f:(fun x -> x)

