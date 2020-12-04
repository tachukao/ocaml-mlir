open Ctypes
module T = Types.Bindings (Types_bindings_generated)

let stringref_callback =
  Foreign.funptr (T.Support.StringRef.t @-> ptr void @-> returning void)


include T
