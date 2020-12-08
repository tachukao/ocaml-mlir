open Core
open IR

(* test run pass on module *)
let%expect_test _ =
  with_context (fun ctx ->
      register_all_dialects ctx;
      let m =
        {|
    func @foo(%arg0 : i32) -> i32 {
        %res = addi %arg0, %arg0 : i32
        return %res : i32 }
    |}
        |> Module.parse ctx
      in
      assert (Module.(not (is_null m)));
      (* Run the print-op-stats pass on the top-level module *)
      let pm = PassManager.create ctx in
      let print_op_stats_pass = Transforms.PrintOpStats.create () in
      PassManager.add_owned_pass pm print_op_stats_pass;
      let success = PassManager.run pm m in
      assert success;
      PassManager.destroy pm;
      Module.destroy m);
  [%expect
    {|
      Operations encountered:
      -----------------------
            func              , 1
            module            , 1
            module_terminator , 1
        std.addi              , 1
        std.return            , 1
      |}]
