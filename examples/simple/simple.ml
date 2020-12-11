open Core
open IR

let () =
  let context = Context.create () in
  Context.num_loaded_dialects context |> Printf.printf "number_of_loaded_dialects %i\n";
  Context.num_registered_dialects context
  |> Printf.printf "number_of_registered_dialects %i\n";
  Context.destroy context
