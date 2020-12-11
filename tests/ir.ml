(* Recreating CAPI test: ir.c *)
open Core
open IR

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
  let loc = Location.unknown ctx in
  make_and_dump_add ctx loc


let build_with_insertions_and_print ctx =
  let loc = Location.unknown ctx in
  let owning_region = Region.create () in
  let null_block = Region.first_block owning_region in
  let state = OperationState.get "insertion.order.test" loc in
  OperationState.add_owned_regions state [ owning_region ];
  let op = Operation.create state in
  let region = Operation.region op 0 in
  (* use integer types of different bitwidth as block arguments in order to differentiate blocks *)
  let i1 = BuiltinTypes.Integer.get ctx 1 in
  let i2 = BuiltinTypes.Integer.get ctx 2 in
  let i3 = BuiltinTypes.Integer.get ctx 3 in
  let i4 = BuiltinTypes.Integer.get ctx 4 in
  let block1 = Block.create [ i1 ] in
  let block2 = Block.create [ i2 ] in
  let block3 = Block.create [ i3 ] in
  let block4 = Block.create [ i4 ] in
  (* insert block as as to obtain the 1-2-3-4 order *)
  Region.insert_owned_block_before region null_block block3;
  Region.insert_owned_block_before region block3 block2;
  Region.insert_owned_block_after region null_block block1;
  Region.insert_owned_block_after region block3 block4;
  let op1_state = OperationState.get "dummy.op1" loc in
  let op2_state = OperationState.get "dummy.op2" loc in
  let op3_state = OperationState.get "dummy.op3" loc in
  let op4_state = OperationState.get "dummy.op4" loc in
  let op5_state = OperationState.get "dummy.op5" loc in
  let op6_state = OperationState.get "dummy.op6" loc in
  let op7_state = OperationState.get "dummy.op7" loc in
  let op1 = Operation.create op1_state in
  let op2 = Operation.create op2_state in
  let op3 = Operation.create op3_state in
  let op4 = Operation.create op4_state in
  let op5 = Operation.create op5_state in
  let op6 = Operation.create op6_state in
  let op7 = Operation.create op7_state in
  (* insert operations in the first block so as to obtain the 1-2-3-4 order *)
  let null_operation = Block.first_operation block1 in
  assert (Operation.is_null null_operation);
  Block.insert_owned_operation_before block1 null_operation op3;
  Block.insert_owned_operation_before block1 op3 op2;
  Block.insert_owned_operation_after block1 null_operation op1;
  Block.insert_owned_operation_after block1 op3 op4;
  (* append operations to the rest of blocks to make them non-empty and thus printable *)
  Block.append_owned_operation block2 op5;
  Block.append_owned_operation block3 op6;
  Block.append_owned_operation block4 op7;
  Operation.dump op;
  Operation.destroy op


(* Dumps instances of all builtin types to check that C API works correctly.
  Additionally, performs simple identity checks that a builtin type
  constructed with C API can be inspected and has the expected type. The
  latter achieves full coverage of C API for builtin types. Returns 0 on
  success and a non-zero error code on failure. *)
let print_builtin_types ctx =
  (* Integer types *)
  let i32 = BuiltinTypes.Integer.get ctx 32 in
  let si32 = BuiltinTypes.Integer.signed ctx 32 in
  let ui32 = BuiltinTypes.Integer.unsigned ctx 32 in
  Printf.printf "@types\n%!";
  Type.dump i32;
  Printf.printf "\n%!";
  Type.dump si32;
  Printf.printf "\n%!";
  Type.dump ui32;
  Printf.printf "\n%!";
  (* Index Type *)
  let index = BuiltinTypes.Index.get ctx in
  Type.dump index;
  Printf.printf "\n%!";
  (* Floating-point types *)
  let bf16 = BuiltinTypes.Float.bf16 ctx in
  let f16 = BuiltinTypes.Float.f16 ctx in
  let f32 = BuiltinTypes.Float.f32 ctx in
  let f64 = BuiltinTypes.Float.f64 ctx in
  Type.dump bf16;
  Printf.printf "\n%!";
  Type.dump f16;
  Printf.printf "\n%!";
  Type.dump f32;
  Printf.printf "\n%!";
  Type.dump f64;
  Printf.printf "\n%!";
  (* None type *)
  let none = BuiltinTypes.None.get ctx in
  Type.dump none;
  Printf.printf "\n%!";
  (* Complex type *)
  let cplx = BuiltinTypes.Complex.get f32 in
  Type.dump cplx;
  Printf.printf "\n%!";
  (* Vector (and Shaped) type. ShapedType is a common base class for vectors,
  memrefs and tensors, one cannot create instances of this class so it is
  tested on an instance of vector type. *)
  let shape = [| 2; 3 |] in
  let vector = BuiltinTypes.Vector.get shape f32 in
  Type.dump vector;
  Printf.printf "\n%!";
  (* Ranked tensor type *)
  let ranked_tensor = BuiltinTypes.Tensor.ranked shape f32 in
  assert (BuiltinTypes.Tensor.is_tensor ranked_tensor);
  assert (BuiltinTypes.Tensor.is_ranked_tensor ranked_tensor);
  assert (not (BuiltinTypes.Tensor.is_unranked_tensor ranked_tensor));
  Type.dump ranked_tensor;
  Printf.printf "\n%!";
  (* Unranked tensor type *)
  let unranked_tensor = BuiltinTypes.Tensor.unranked f32 in
  assert (BuiltinTypes.Tensor.is_tensor unranked_tensor);
  assert (not (BuiltinTypes.Tensor.is_ranked_tensor unranked_tensor));
  assert (BuiltinTypes.Tensor.is_unranked_tensor unranked_tensor);
  Type.dump unranked_tensor;
  Printf.printf "\n%!";
  (* MemRef type *)
  let memref = BuiltinTypes.MemRef.contiguous f32 shape 2 in
  Type.dump memref;
  Printf.printf "\n%!";
  (* Unranked MemRef type *)
  let unranked_memref = BuiltinTypes.MemRef.unranked f32 4 in
  Type.dump unranked_memref;
  Printf.printf "\n%!";
  (* Tuple Type *)
  let types = [ unranked_memref; f32 ] in
  let tuple = BuiltinTypes.Tuple.get ctx types in
  assert (BuiltinTypes.Tuple.(is_tuple tuple));
  assert (BuiltinTypes.Tuple.(num_types tuple = 2));
  assert (Type.(equal BuiltinTypes.Tuple.(nth tuple 0) unranked_memref));
  assert (Type.(equal BuiltinTypes.Tuple.(nth tuple 1) f32));
  Type.dump tuple;
  Printf.printf "\n%!";
  let func_inputs = [ BuiltinTypes.Index.get ctx; BuiltinTypes.Integer.get ctx 1 ] in
  let func_results =
    [ BuiltinTypes.Integer.get ctx 16
    ; BuiltinTypes.Integer.get ctx 32
    ; BuiltinTypes.Integer.get ctx 64
    ]
  in
  let func_type =
    BuiltinTypes.Function.get ~inputs:func_inputs ~results:func_results ctx
  in
  assert (BuiltinTypes.Function.num_inputs func_type = 2);
  assert (BuiltinTypes.Function.num_results func_type = 3);
  assert (Type.equal BuiltinTypes.Function.(result func_type 0) List.(nth func_results 0));
  assert (Type.equal BuiltinTypes.Function.(input func_type 1) List.(nth func_inputs 1));
  Type.dump func_type;
  Printf.printf "\n%!"


let print_builtin_attributes ctx =
  (* float *)
  let floating = BuiltinAttributes.Float.get ctx BuiltinTypes.Float.(f64 ctx) 2. in
  assert (BuiltinAttributes.Float.(is_float floating));
  assert (Float.abs (BuiltinAttributes.Float.(value floating) -. 2.) < 1E-6);
  Printf.printf "@attrs\n%!";
  Attribute.dump floating;
  let floating_type = Attribute.get_type floating in
  Type.dump floating_type;
  print_newline ();
  (* integer *)
  let integer = BuiltinAttributes.Integer.get BuiltinTypes.Integer.(get ctx 32) 42 in
  assert (BuiltinAttributes.Integer.(is_integer integer));
  assert (BuiltinAttributes.Integer.(value integer) = 42);
  Attribute.dump integer;
  (* bool *)
  let boolean = BuiltinAttributes.Bool.get ctx 1 in
  assert (BuiltinAttributes.Bool.(is_bool boolean));
  assert (BuiltinAttributes.Bool.(value boolean));
  Attribute.dump boolean;
  (* opaque *)
  let data = "abc" in
  let opaque = BuiltinAttributes.Opaque.get ctx "std" data BuiltinTypes.None.(get ctx) in
  assert (BuiltinAttributes.Opaque.(is_opaque opaque));
  assert (BuiltinAttributes.Opaque.(namespace opaque) = "std");
  let opaque_data = BuiltinAttributes.Opaque.data opaque in
  assert (String.length opaque_data = 3);
  assert (String.length opaque_data = String.length data);
  Attribute.dump opaque;
  ignore ctx


let print_affine_map ctx = ignore ctx
let print_affine_expr ctx = ignore ctx

let register_only_std ctx =
  assert (Context.(num_loaded_dialects ctx = 1));
  let std = Context.get_or_load_dialect ctx StandardDialect.(namespace ()) in
  assert (Dialect.is_null std);
  StandardDialect.register_standard_dialect ctx;
  assert (Context.num_registered_dialects ctx = 1);
  assert (Context.num_loaded_dialects ctx = 1);
  let std = Context.get_or_load_dialect ctx StandardDialect.(namespace ()) in
  assert (not (Dialect.is_null std));
  assert (Context.num_loaded_dialects ctx = 2);
  let also_std = StandardDialect.load_standard_dialect ctx in
  assert (Dialect.(equal std also_std));
  let std_ns = Dialect.namespace std in
  let also_std_ns = StandardDialect.namespace () in
  assert (std_ns = also_std_ns)


let%expect_test _ =
  with_context (fun ctx ->
      register_all_dialects ctx;
      construct_and_traverse_ir ctx);
  [%expect
    {|
      module  {
        func @add(%arg0: memref<?xf32>, %arg1: memref<?xf32>) {
          %c0 = constant 0 : index
          %0 = dim %arg0, %c0 : memref<?xf32>
          %c1 = constant 1 : index
          scf.for %arg2 = %c0 to %0 step %c1 {
            %1 = load %arg0[%arg2] : memref<?xf32>
            %2 = load %arg1[%arg2] : memref<?xf32>
            %3 = addf %1, %2 : f32
            store %3, %arg0[%arg2] : memref<?xf32>
          }
          return
        }
      }
    |}]

let%expect_test _ =
  with_context (fun ctx -> build_with_insertions_and_print ctx);
  [%expect
    {|
    "insertion.order.test"() ( {
    ^bb0(%arg0: i1):  // no predecessors
      "dummy.op1"() : () -> ()
      "dummy.op2"() : () -> ()
      "dummy.op3"() : () -> ()
      "dummy.op4"() : () -> ()
    ^bb1(%0: i2):  // no predecessors
      "dummy.op5"() : () -> ()
    ^bb2(%1: i3):  // no predecessors
      "dummy.op6"() : () -> ()
    ^bb3(%2: i4):  // no predecessors
      "dummy.op7"() : () -> ()
    }) : () -> ()
    |}]

let%expect_test _ =
  with_context print_builtin_types;
  [%expect
    {|
  @types
  i32
  si32
  ui32
  index
  bf16
  f16
  f32
  f64
  none
  complex<f32>
  vector<2x3xf32>
  tensor<2x3xf32>
  tensor<*xf32>
  memref<2x3xf32, 2>
  memref<*xf32, 4>
  tuple<memref<*xf32, 4>, f32>
  (index, i1) -> (i16, i32, i64)
  |}]

let%expect_test _ =
  with_context print_builtin_attributes;
  [%expect {|
  @attrs
  2.000000e+00 : f64
  f64
  42 : i32
  true 
  #std.abc
  |}]

let%expect_test _ =
  with_context print_affine_map;
  [%expect {|
  |}]

let%expect_test _ =
  with_context print_affine_expr;
  [%expect {|
  |}]

let%test_unit _ = with_context register_only_std
