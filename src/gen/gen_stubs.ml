let with_file ~f file =
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  f fmt;
  close_out oc


let () =
  with_file "mlir_stubs.c" ~f:(fun fmt_c ->
      Format.fprintf fmt_c "#include \"mlir-c/AffineExpr.h\"@.";
      Format.fprintf fmt_c "#include \"mlir-c/AffineMap.h\"@.";
      Format.fprintf fmt_c "#include \"mlir-c/Diagnostics.h\"@.";
      Format.fprintf fmt_c "#include \"mlir-c/IR.h\"@.";
      Format.fprintf fmt_c "#include \"mlir-c/Pass.h\"@.";
      Format.fprintf fmt_c "#include \"mlir-c/Registration.h\"@.";
      Format.fprintf fmt_c "#include \"mlir-c/BuiltinAttributes.h\"@.";
      Format.fprintf fmt_c "#include \"mlir-c/BuiltinTypes.h\"@.";
      Format.fprintf fmt_c "#include \"mlir-c/StandardDialect.h\"@.";
      Cstubs.write_c fmt_c ~prefix:"caml" (module Stubs.Bindings));
  with_file "mlir_generated.ml" ~f:(fun fmt_ml ->
      Cstubs.write_ml fmt_ml ~prefix:"caml" (module Stubs.Bindings));
  flush_all ()
