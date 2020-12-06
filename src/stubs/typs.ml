open Ctypes
module T = Types.Bindings (Types_bindings_generated)

let string_callback = Foreign.funptr (T.StringRef.t @-> ptr void @-> returning void)

let diagnostic_handler =
  Foreign.funptr (T.Diagnostic.t @-> ptr void @-> returning T.LogicalResult.t)


include T
