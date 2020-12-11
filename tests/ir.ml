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
  assert (opaque_data = data);
  Attribute.dump opaque;
  (* string *)
  let data = "de" in
  let str = BuiltinAttributes.String.get ctx data in
  assert (BuiltinAttributes.String.(is_string str));
  let str_value = BuiltinAttributes.String.value str in
  assert (String.length str_value = 2);
  assert (str_value = data);
  Attribute.dump str;
  (* flat symbol *)
  let flat_symbol_ref = BuiltinAttributes.FlatSymbolRef.get ctx "fgh" in
  assert (BuiltinAttributes.FlatSymbolRef.(is_flat_symbol_ref flat_symbol_ref));
  let flat_symbol_ref_value = BuiltinAttributes.FlatSymbolRef.value flat_symbol_ref in
  assert (String.length flat_symbol_ref_value = 3);
  assert (flat_symbol_ref_value = "fgh");
  Attribute.dump flat_symbol_ref;
  (* symbols *)
  let symbols = [ flat_symbol_ref; flat_symbol_ref ] in
  let symbol_ref = BuiltinAttributes.SymbolRef.get ctx "ij" symbols in
  assert (BuiltinAttributes.SymbolRef.is_symbol_ref symbol_ref);
  assert (BuiltinAttributes.SymbolRef.num_nested_refs symbol_ref = 2);
  assert (
    Attribute.equal BuiltinAttributes.SymbolRef.(nested_ref symbol_ref 0) flat_symbol_ref
  );
  assert (
    Attribute.equal BuiltinAttributes.SymbolRef.(nested_ref symbol_ref 1) flat_symbol_ref
  );
  let symbol_ref_leaf = BuiltinAttributes.SymbolRef.leaf_ref symbol_ref in
  let symbol_ref_root = BuiltinAttributes.SymbolRef.root_ref symbol_ref in
  assert (String.length symbol_ref_leaf = 3);
  assert (String.length symbol_ref_root = 2);
  assert (symbol_ref_leaf = "fgh");
  assert (symbol_ref_root = "ij");
  Attribute.dump symbol_ref;
  (* type *)
  let typ = BuiltinAttributes.Type.get BuiltinTypes.(Float.f32 ctx) in
  assert (BuiltinAttributes.Type.is_type typ);
  assert (Type.equal BuiltinTypes.(Float.f32 ctx) BuiltinAttributes.Type.(value typ));
  Attribute.dump typ;
  (* unit *)
  let u = BuiltinAttributes.Unit.get ctx in
  assert (BuiltinAttributes.Unit.is_unit u);
  Attribute.dump u;
  let shp = [| 1; 2 |] in
  let bools = [ 0; 1 ] in
  let uints32 = [ 0; 1 ] in
  let ints32 = [ 0; 1 ] in
  let uints64 = [ 0; 1 ] in
  let ints64 = [ 0; 1 ] in
  let floats = [ 0.; 1. ] in
  let doubles = [ 0.; 1. ] in
  let bool_elements =
    BuiltinAttributes.Elements.Dense.bool_get
      BuiltinTypes.(Tensor.ranked shp Integer.(get ctx 1))
      bools
  in
  let uint32_elements =
    BuiltinAttributes.Elements.Dense.uint32_get
      BuiltinTypes.(Tensor.ranked shp Integer.(unsigned ctx 32))
      uints32
  in
  let int32_elements =
    BuiltinAttributes.Elements.Dense.int32_get
      BuiltinTypes.(Tensor.ranked shp Integer.(get ctx 32))
      ints32
  in
  let uint64_elements =
    BuiltinAttributes.Elements.Dense.uint64_get
      BuiltinTypes.(Tensor.ranked shp Integer.(unsigned ctx 64))
      uints64
  in
  let int64_elements =
    BuiltinAttributes.Elements.Dense.int64_get
      BuiltinTypes.(Tensor.ranked shp Integer.(get ctx 64))
      ints64
  in
  let float_elements =
    BuiltinAttributes.Elements.Dense.float_get
      BuiltinTypes.(Tensor.ranked shp Float.(f32 ctx))
      floats
  in
  let double_elements =
    BuiltinAttributes.Elements.Dense.double_get
      BuiltinTypes.(Tensor.ranked shp Float.(f64 ctx))
      doubles
  in
  (* is dense *)
  assert (BuiltinAttributes.Elements.Dense.is_dense bool_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense uint32_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense int32_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense uint64_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense int64_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense float_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense double_elements);
  (* Get value *)
  assert (BuiltinAttributes.Elements.Dense.(bool_value bool_elements 1) = true);
  assert (BuiltinAttributes.Elements.Dense.(uint32_value uint32_elements 1) = 1);
  assert (BuiltinAttributes.Elements.Dense.(int32_value int32_elements 1) = 1);
  assert (BuiltinAttributes.Elements.Dense.(uint64_value uint64_elements 1) = 1);
  assert (BuiltinAttributes.Elements.Dense.(int64_value int64_elements 1) = 1);
  assert (
    Float.(abs (BuiltinAttributes.Elements.Dense.(float_value float_elements 1) -. 1.))
    < 1E-6);
  assert (
    Float.(abs (BuiltinAttributes.Elements.Dense.(double_value double_elements 1) -. 1.))
    < 1E-6);
  (* dump *)
  Attribute.dump bool_elements;
  Attribute.dump uint32_elements;
  Attribute.dump int32_elements;
  Attribute.dump uint64_elements;
  Attribute.dump int64_elements;
  Attribute.dump float_elements;
  Attribute.dump double_elements;
  (* SPLAT *)
  let bool_elements =
    BuiltinAttributes.Elements.Dense.bool_splat_get
      BuiltinTypes.(Tensor.ranked shp Integer.(get ctx 1))
      true
  in
  let uint32_elements =
    BuiltinAttributes.Elements.Dense.uint32_splat_get
      BuiltinTypes.(Tensor.ranked shp Integer.(unsigned ctx 32))
      1
  in
  let int32_elements =
    BuiltinAttributes.Elements.Dense.int32_splat_get
      BuiltinTypes.(Tensor.ranked shp Integer.(get ctx 32))
      1
  in
  let uint64_elements =
    BuiltinAttributes.Elements.Dense.uint64_splat_get
      BuiltinTypes.(Tensor.ranked shp Integer.(unsigned ctx 64))
      1
  in
  let int64_elements =
    BuiltinAttributes.Elements.Dense.int64_splat_get
      BuiltinTypes.(Tensor.ranked shp Integer.(get ctx 64))
      1
  in
  let float_elements =
    BuiltinAttributes.Elements.Dense.float_splat_get
      BuiltinTypes.(Tensor.ranked shp Float.(f32 ctx))
      1.
  in
  let double_elements =
    BuiltinAttributes.Elements.Dense.double_splat_get
      BuiltinTypes.(Tensor.ranked shp Float.(f64 ctx))
      1.
  in
  (* is dense *)
  assert (BuiltinAttributes.Elements.Dense.is_dense bool_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense uint32_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense int32_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense uint64_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense int64_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense float_elements);
  assert (BuiltinAttributes.Elements.Dense.is_dense double_elements);
  (* is splat *)
  assert (BuiltinAttributes.Elements.Dense.is_splat bool_elements);
  assert (BuiltinAttributes.Elements.Dense.is_splat uint32_elements);
  assert (BuiltinAttributes.Elements.Dense.is_splat int32_elements);
  assert (BuiltinAttributes.Elements.Dense.is_splat uint64_elements);
  assert (BuiltinAttributes.Elements.Dense.is_splat int64_elements);
  assert (BuiltinAttributes.Elements.Dense.is_splat float_elements);
  assert (BuiltinAttributes.Elements.Dense.is_splat double_elements);
  (* Get value *)
  assert (BuiltinAttributes.Elements.Dense.(bool_splat_value bool_elements) = 1);
  assert (BuiltinAttributes.Elements.Dense.(uint32_splat_value uint32_elements) = 1);
  assert (BuiltinAttributes.Elements.Dense.(int32_splat_value int32_elements) = 1);
  assert (BuiltinAttributes.Elements.Dense.(uint64_splat_value uint64_elements) = 1);
  assert (BuiltinAttributes.Elements.Dense.(int64_splat_value int64_elements) = 1);
  assert (
    Float.(
      abs (BuiltinAttributes.Elements.Dense.(float_splat_value float_elements) -. 1.))
    < 1E-6);
  assert (
    Float.(
      abs (BuiltinAttributes.Elements.Dense.(double_splat_value double_elements) -. 1.))
    < 1E-6);
  (* dump *)
  Attribute.dump bool_elements;
  Attribute.dump uint32_elements;
  Attribute.dump int32_elements;
  Attribute.dump uint64_elements;
  Attribute.dump int64_elements;
  Attribute.dump float_elements;
  Attribute.dump double_elements;
  (* sparse *)
  let indices =
    BuiltinAttributes.Elements.Dense.int64_get
      BuiltinTypes.(Tensor.ranked [| 2 |] Integer.(get ctx 64))
      [ 4; 7 ]
  in
  let values =
    BuiltinAttributes.Elements.Dense.float_get
      BuiltinTypes.(Tensor.ranked [| 2 |] Float.(f32 ctx))
      floats
  in
  let sparse =
    BuiltinAttributes.Elements.Sparse.create
      BuiltinTypes.(Tensor.ranked [| 2 |] Float.(f32 ctx))
      indices
      values
  in
  Attribute.dump sparse


let print_affine_map ctx =
  let empty = AffineMap.empty ctx in
  let affine_map = AffineMap.get ctx 3 2 in
  let const = AffineMap.constant ctx 2 in
  let multi_dim_identity = AffineMap.multi_dim_identity ctx 3 in
  let minor_identity = AffineMap.minor_identity ctx 3 2 in
  let perm = [ 1; 2; 0 ] in
  let permutation = AffineMap.permutation ctx perm in
  Printf.printf "@affineMap\n%!";
  AffineMap.dump empty;
  AffineMap.dump affine_map;
  AffineMap.dump const;
  AffineMap.dump multi_dim_identity;
  AffineMap.dump minor_identity;
  AffineMap.dump permutation;
  (* is identity *)
  assert (AffineMap.is_identity empty);
  assert (not (AffineMap.is_identity affine_map));
  assert (not (AffineMap.is_identity const));
  assert (AffineMap.is_identity multi_dim_identity);
  assert (not (AffineMap.is_identity minor_identity));
  assert (not (AffineMap.is_identity permutation));
  (* is minor identity*)
  assert (AffineMap.is_minor_identity empty);
  assert (not (AffineMap.is_minor_identity affine_map));
  assert (AffineMap.is_minor_identity multi_dim_identity);
  assert (AffineMap.is_minor_identity minor_identity);
  assert (not (AffineMap.is_minor_identity permutation));
  (* is empty *)
  assert (AffineMap.is_empty empty);
  assert (not (AffineMap.is_empty affine_map));
  assert (not (AffineMap.is_identity const));
  assert (not (AffineMap.is_empty multi_dim_identity));
  assert (not (AffineMap.is_empty minor_identity));
  assert (not (AffineMap.is_empty permutation));
  (* is single constant *)
  assert (not (AffineMap.is_single_constant empty));
  assert (not (AffineMap.is_single_constant affine_map));
  assert (AffineMap.is_single_constant const);
  assert (not (AffineMap.is_single_constant multi_dim_identity));
  assert (not (AffineMap.is_single_constant minor_identity));
  assert (not (AffineMap.is_single_constant permutation));
  (* get const reusult *)
  assert (AffineMap.single_constant_result const = 2);
  (* get num dims *)
  assert (AffineMap.num_dims empty = 0);
  assert (AffineMap.num_dims affine_map = 3);
  assert (AffineMap.num_dims const = 0);
  assert (AffineMap.num_dims multi_dim_identity = 3);
  assert (AffineMap.num_dims minor_identity = 3);
  assert (AffineMap.num_dims permutation = 3);
  (* get num symbols *)
  assert (AffineMap.num_symbols empty = 0);
  assert (AffineMap.num_symbols affine_map = 2);
  assert (AffineMap.num_symbols const = 0);
  assert (AffineMap.num_symbols multi_dim_identity = 0);
  assert (AffineMap.num_symbols minor_identity = 0);
  assert (AffineMap.num_symbols permutation = 0);
  (* get num results *)
  assert (AffineMap.num_results empty = 0);
  assert (AffineMap.num_results affine_map = 0);
  assert (AffineMap.num_results const = 1);
  assert (AffineMap.num_results multi_dim_identity = 3);
  assert (AffineMap.num_results minor_identity = 2);
  assert (AffineMap.num_results permutation = 3);
  (* get num inputs *)
  assert (AffineMap.num_inputs empty = 0);
  assert (AffineMap.num_inputs affine_map = 5);
  assert (AffineMap.num_inputs const = 0);
  assert (AffineMap.num_inputs multi_dim_identity = 3);
  assert (AffineMap.num_inputs minor_identity = 3);
  assert (AffineMap.num_inputs permutation = 3);
  (* is projected permutation *)
  assert (AffineMap.is_projected_permutation empty);
  assert (not (AffineMap.is_projected_permutation affine_map));
  assert (not (AffineMap.is_projected_permutation const));
  assert (AffineMap.is_projected_permutation multi_dim_identity);
  assert (AffineMap.is_projected_permutation minor_identity);
  assert (AffineMap.is_projected_permutation permutation);
  (* is permutation *)
  assert (AffineMap.is_permutation empty);
  assert (not (AffineMap.is_permutation affine_map));
  assert (not (AffineMap.is_permutation const));
  assert (AffineMap.is_permutation multi_dim_identity);
  assert (not (AffineMap.is_permutation minor_identity));
  assert (AffineMap.is_permutation permutation);
  let sub_map = AffineMap.sub_map multi_dim_identity [ 1 ] in
  let major_sub_map = AffineMap.major_sub_map multi_dim_identity 1 in
  let minor_sub_map = AffineMap.minor_sub_map multi_dim_identity 1 in
  AffineMap.dump sub_map;
  AffineMap.dump major_sub_map;
  AffineMap.dump minor_sub_map


let print_affine_expr ctx =
  let dim = AffineExpr.Dimension.get ctx 5 in
  let symbol = AffineExpr.Symbol.get ctx 5 in
  let const = AffineExpr.Constant.get ctx 5 in
  let add = AffineExpr.Add.get dim symbol in
  let mul = AffineExpr.Mul.get dim symbol in
  let modu = AffineExpr.Mod.get dim symbol in
  let floordiv = AffineExpr.FloorDiv.get dim symbol in
  let ceildiv = AffineExpr.CeilDiv.get dim symbol in
  Printf.printf "@affineExpr\n%!";
  AffineExpr.dump dim;
  AffineExpr.dump symbol;
  AffineExpr.dump const;
  AffineExpr.dump add;
  AffineExpr.dump mul;
  AffineExpr.dump modu;
  AffineExpr.dump floordiv;
  AffineExpr.dump ceildiv;
  AffineExpr.dump AffineExpr.BinaryOp.(lhs add);
  AffineExpr.dump AffineExpr.BinaryOp.(rhs add);
  assert (AffineExpr.Dimension.position dim = 5);
  assert (AffineExpr.Symbol.position symbol = 5);
  assert (AffineExpr.Constant.value const = 5);
  assert (not AffineExpr.(is_symbolic_or_constant dim));
  assert (AffineExpr.(is_symbolic_or_constant symbol));
  assert (AffineExpr.(is_symbolic_or_constant const));
  assert (not AffineExpr.(is_symbolic_or_constant add));
  assert (not AffineExpr.(is_symbolic_or_constant mul));
  assert (not AffineExpr.(is_symbolic_or_constant modu));
  assert (not AffineExpr.(is_symbolic_or_constant floordiv));
  assert (not AffineExpr.(is_symbolic_or_constant ceildiv));
  assert (AffineExpr.(is_pure_affine dim));
  assert (AffineExpr.(is_pure_affine symbol));
  assert (AffineExpr.(is_pure_affine const));
  assert (AffineExpr.(is_pure_affine add));
  assert (not AffineExpr.(is_pure_affine mul));
  assert (not AffineExpr.(is_pure_affine modu));
  assert (not AffineExpr.(is_pure_affine floordiv));
  assert (not AffineExpr.(is_pure_affine ceildiv));
  assert (not AffineExpr.(largest_known_divisor dim <> 1));
  assert (not AffineExpr.(largest_known_divisor symbol <> 1));
  assert (not AffineExpr.(largest_known_divisor const <> 5));
  assert (not AffineExpr.(largest_known_divisor add <> 1));
  assert (not AffineExpr.(largest_known_divisor mul <> 1));
  assert (not AffineExpr.(largest_known_divisor modu <> 1));
  assert (not AffineExpr.(largest_known_divisor floordiv <> 1));
  assert (not AffineExpr.(largest_known_divisor ceildiv <> 1));
  assert (AffineExpr.(is_multiple_of dim 1));
  assert (AffineExpr.(is_multiple_of symbol 1));
  assert (AffineExpr.(is_multiple_of const 5));
  assert (AffineExpr.(is_multiple_of add 1));
  assert (AffineExpr.(is_multiple_of mul 1));
  assert (AffineExpr.(is_multiple_of modu 1));
  assert (AffineExpr.(is_multiple_of floordiv 1));
  assert (AffineExpr.(is_multiple_of ceildiv 1));
  assert (AffineExpr.(is_function_of_dim dim 5));
  assert (not AffineExpr.(is_function_of_dim symbol 5));
  assert (not AffineExpr.(is_function_of_dim const 5));
  assert (AffineExpr.(is_function_of_dim add 5));
  assert (AffineExpr.(is_function_of_dim mul 5));
  assert (AffineExpr.(is_function_of_dim modu 5));
  assert (AffineExpr.(is_function_of_dim floordiv 5));
  assert (AffineExpr.(is_function_of_dim ceildiv 5));
  assert (AffineExpr.Add.is_add add);
  assert (AffineExpr.Mul.is_mul mul);
  assert (AffineExpr.FloorDiv.is_floor_div floordiv);
  assert (AffineExpr.CeilDiv.is_ceil_div ceildiv)


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
  [%expect
    {|
  @attrs
  2.000000e+00 : f64
  f64
  42 : i32
  true 
  #std.abc
  "de"
  @fgh
  @ij::@fgh::@fgh
  f32
  unit
  dense<[[false, true]]> : tensor<1x2xi1>
  dense<[[0, 1]]> : tensor<1x2xui32>
  dense<[[0, 1]]> : tensor<1x2xi32>
  dense<[[0, 1]]> : tensor<1x2xui64>
  dense<[[0, 1]]> : tensor<1x2xi64>
  dense<[[0.000000e+00, 1.000000e+00]]> : tensor<1x2xf32>
  dense<[[0.000000e+00, 1.000000e+00]]> : tensor<1x2xf64>
  dense<true> : tensor<1x2xi1>
  dense<1> : tensor<1x2xui32>
  dense<1> : tensor<1x2xi32>
  dense<1> : tensor<1x2xui64>
  dense<1> : tensor<1x2xi64>
  dense<1.000000e+00> : tensor<1x2xf32>
  dense<1.000000e+00> : tensor<1x2xf64>
  sparse<[4, 7], [0.000000e+00, 1.000000e+00]> : tensor<2xf32>
  |}]

let%expect_test _ =
  with_context print_affine_map;
  [%expect
    {|
  @affineMap
  () -> ()
  (d0, d1, d2)[s0, s1] -> ()
  () -> (2)
  (d0, d1, d2) -> (d0, d1, d2)
  (d0, d1, d2) -> (d1, d2)
  (d0, d1, d2) -> (d1, d2, d0)
  (d0, d1, d2) -> (d1)
  (d0, d1, d2) -> (d0)
  (d0, d1, d2) -> (d2)
  |}]

let%expect_test _ =
  with_context print_affine_expr;
  [%expect
    {|
  @affineExpr
  d5
  s5
  5
  d5 + s5
  d5 * s5
  d5 mod s5
  d5 floordiv s5
  d5 ceildiv s5
  d5
  s5 |}]

let%test_unit _ = with_context register_only_std
