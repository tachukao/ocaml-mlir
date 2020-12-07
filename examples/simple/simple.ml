open Core

let context = Context.create ()

let () =
  Context.num_loaded_dialects context |> Printf.printf "number_of_loaded_dialects %i\n";
  Context.num_registered_dialects context
  |> Printf.printf "number_of_registered_dialects %i\n"
