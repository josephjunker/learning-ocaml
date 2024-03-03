open Base

let () = Random.init 123

let zobrist_dictionary = Zobrist.init_zobrist ()

let initial_board_state = InitialSetup.initial_board_state
