open Ctypes
module T = Types.Bindings (Types_bindings_generated)

let string_callback = Foreign.funptr (T.StringRef.t @-> ptr void @-> returning void)

include T
