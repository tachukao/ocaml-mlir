(* Recreating CAPI test: pass.c *)
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

(* test run pass on nested module *)
let%expect_test _ =
  with_context (fun ctx ->
      register_all_dialects ctx;
      let m =
        Module.parse
          ctx
          {|
      func @foo(%arg0 : i32) -> i32{
        %res = addi %arg0, %arg0 :i32
        return %res : i32
        }
      module{
        func @bar(%arg0 : f32) -> f32 {
        %res = addf %arg0, %arg0 : f32
        return %res : f32
        }
      }
      |}
      in
      assert (not Module.(is_null m));
      let pm = PassManager.create ctx in
      let nested_func_pm = PassManager.nested_under pm "func" in
      let print_op_stat_pass = Transforms.PrintOpStats.create () in
      OpPassManager.add_owned_pass nested_func_pm print_op_stat_pass;
      let success = PassManager.run pm m in
      assert success;
      PassManager.destroy pm;
      let pm = PassManager.create ctx in
      let nested_module_pm = PassManager.nested_under pm "module" in
      let nested_func_pm = OpPassManager.nested_under nested_module_pm "func" in
      let print_op_stat_pass = Transforms.PrintOpStats.create () in
      OpPassManager.add_owned_pass nested_func_pm print_op_stat_pass;
      let success = PassManager.run pm m in
      assert success;
      PassManager.destroy pm);
  [%expect
    {|
      Operations encountered:
      -----------------------
            func   , 1
        std.addi   , 1
        std.return , 1
      Operations encountered:
      -----------------------
            func   , 1
        std.addf   , 1
        std.return , 1
      |}]

let%expect_test _ =
  with_context (fun ctx ->
      let pm = PassManager.create ctx in
      let nested_module_pm = PassManager.nested_under pm "module" in
      let nested_func_pm = OpPassManager.nested_under nested_module_pm "func" in
      let print_op_stat_pass = Transforms.PrintOpStats.create () in
      OpPassManager.add_owned_pass nested_func_pm print_op_stat_pass;
      Printf.printf "Top-level: %!";
      OpPassManager.print_pass_pipeline
        ~callback:print_string
        PassManager.(to_op_pass_manager pm);
      Printf.printf "\n%!";
      Printf.printf "Nested Module: %!";
      OpPassManager.print_pass_pipeline ~callback:print_string nested_module_pm;
      Printf.printf "\n%!";
      Printf.printf "Nested Module>Func: %!";
      OpPassManager.print_pass_pipeline ~callback:print_string nested_func_pm;
      Printf.printf "\n%!";
      PassManager.destroy pm);
  [%expect
    {|
      Top-level: module(func(print-op-stats))
      Nested Module: func(print-op-stats)
      Nested Module>Func: print-op-stats
      |}]
