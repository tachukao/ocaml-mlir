module Typs = Stubs.Typs
module Bindings = Stubs.Bindings (Mlir_generated)

module Context = struct
  type t = (Typs.Context.t, [ `Struct ]) Ctypes_static.structured

  let create = Bindings.Context.create

  let num_loaded_dialects ctx =
    Bindings.Context.num_loaded_dialects ctx |> Signed.Long.to_int
end
