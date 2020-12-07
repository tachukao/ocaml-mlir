open Wrapper

let populate_loop_body ctx loop_body loc func_body =
  let iv = Block.argument loop_body 0 in
  let func_arg0 = Block.argument func_body 0 in
  let func_arg1 = Block.argument func_body 1 in
  let f32_type = Type.parse ctx "f32" in
  (* lhs *)
  let load_lhs_state = OperationState.get "std.load" loc in
  let load_lhs_operands = [ func_arg0; iv ] in
  OperationState.add_operands load_lhs_state load_lhs_operands;
  OperationState.add_results load_lhs_state [ f32_type ];
  let load_lhs = Operation.create load_lhs_state in
  Block.append_owned_operation loop_body load_lhs;
  (* rhs *)
  let load_rhs_state = OperationState.get "std.load" loc in
  let load_rhs_operands = [ func_arg1; iv ] in
  OperationState.add_operands load_rhs_state load_rhs_operands;
  OperationState.add_results load_rhs_state [ f32_type ];
  let load_rhs = Operation.create load_rhs_state in
  Block.append_owned_operation loop_body load_rhs;
  (* addf *)
  let add_state = OperationState.get "std.addf" loc in
  let add_operands = [ Operation.result load_lhs 0; Operation.result load_rhs 0 ] in
  OperationState.add_operands add_state add_operands;
  OperationState.add_results add_state [ f32_type ];
  let add = Operation.create add_state in
  Block.append_owned_operation loop_body add;
  (* store state *)
  let store_state = OperationState.get "std.store" loc in
  let store_operands = [ Operation.result add 0; func_arg0; iv ] in
  OperationState.add_operands store_state store_operands;
  let store = Operation.create store_state in
  Block.append_owned_operation loop_body store;
  let yield_state = OperationState.get "scf.yield" loc in
  let yield = Operation.create yield_state in
  Block.append_owned_operation loop_body yield


let make_and_dump_add ctx loc =
  let module_op = Module.empty loc in
  let module_body = Module.body module_op in
  let memref_type = Type.parse ctx "memref<?xf32>" in
  let func_body_arg_types = [ memref_type; memref_type ] in
  let func_region = Region.create () in
  let func_body = Block.create func_body_arg_types in
  Region.append_owned_block func_region func_body;
  let func_type_attr = Attribute.parse ctx "(memref<?xf32>, memref<?xf32>) -> ()" in
  let func_name_attr = Attribute.parse ctx "\"add\"" in
  let func_attrs =
    [ Attribute.name "type" func_type_attr; Attribute.name "sym_name" func_name_attr ]
  in
  let func_state = OperationState.get "func" loc in
  OperationState.add_named_attributes func_state func_attrs;
  OperationState.add_owned_regions func_state [ func_region ];
  let func = Operation.create func_state in
  Block.insert_owned_operation module_body 0 func;
  let index_type = Type.parse ctx "index" in
  let index_zero_literal = Attribute.parse ctx "0: index" in
  let index_zero_value_attr = Attribute.name "value" index_zero_literal in
  let const_zero_state = OperationState.get "std.constant" loc in
  OperationState.add_results const_zero_state [ index_type ];
  OperationState.add_named_attributes const_zero_state [ index_zero_value_attr ];
  let const_zero = Operation.create const_zero_state in
  Block.append_owned_operation func_body const_zero;
  let func_arg0 = Block.argument func_body 0 in
  let const_zero_value = Operation.result const_zero 0 in
  let dim_operands = [ func_arg0; const_zero_value ] in
  let dim_state = OperationState.get "std.dim" loc in
  OperationState.add_operands dim_state dim_operands;
  OperationState.add_results dim_state [ index_type ];
  let dim = Operation.create dim_state in
  Block.append_owned_operation func_body dim;
  let loop_body_region = Region.create () in
  let loop_body = Block.create [ index_type ] in
  Region.append_owned_block loop_body_region loop_body;
  let index_one_literal = Attribute.parse ctx "1: index" in
  let index_one_value_attr = Attribute.name "value" index_one_literal in
  let const_one_state = OperationState.get "std.constant" loc in
  OperationState.add_results const_one_state [ index_type ];
  OperationState.add_named_attributes const_one_state [ index_one_value_attr ];
  let const_one = Operation.create const_one_state in
  Block.append_owned_operation func_body const_one;
  let dim_value = Operation.result dim 0 in
  let const_one_value = Operation.result const_one 0 in
  let loop_operands = [ const_zero_value; dim_value; const_one_value ] in
  let loop_state = OperationState.get "scf.for" loc in
  OperationState.add_operands loop_state loop_operands;
  OperationState.add_owned_regions loop_state [ loop_body_region ];
  let loop = Operation.create loop_state in
  Block.append_owned_operation func_body loop;
  populate_loop_body ctx loop_body loc func_body;
  let ret_state = OperationState.get "std.return" loc in
  let ret = Operation.create ret_state in
  Block.append_owned_operation func_body ret;
  let modu = Module.operation module_op in
  Operation.dump modu;
  ()


let construct_and_traverse_ir ctx =
  let loc = Location.unknown_get ctx in
  make_and_dump_add ctx loc


let f ctx =
  register_all_dialects ctx;
  construct_and_traverse_ir ctx


let test () =
  with_context (fun ctx -> f ctx);
  Alcotest.(check bool) "dummy" true true


let test_set = [ "runs", `Quick, test ]
