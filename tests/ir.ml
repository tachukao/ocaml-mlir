open Wrapper

let make_and_dump_add ctx loc =
  let op = Module.empty loc in
  let body = Module.body op in
  let memref_typ = Type.parse ctx "memref<?xf32>" in
  ignore memref_typ;
  ignore body;
  ignore op


let construct_and_traverse_ir ctx =
  let loc = Location.unknown_get ctx in
  ignore loc


let test () =
  let ctx = Context.create () in
  register_all_dialects ctx;
  Alcotest.(check bool) "dummy" true true


let test_set = [ "runs", `Quick, test ]
