module Typs = Stubs.Typs
module Bindings = Stubs.Bindings (Mlir_generated)

type 'a structured = ('a, [ `Struct ]) Ctypes_static.structured

module Context = struct
  type t = Typs.Context.t structured

  let create = Bindings.Context.create
  let equal = Bindings.Context.equal
  let is_null = Bindings.Context.is_null

  let num_registered_dialects ctx =
    Bindings.Context.num_registered_dialects ctx |> Signed.Long.to_int


  let num_loaded_dialects ctx =
    Bindings.Context.num_loaded_dialects ctx |> Signed.Long.to_int
end

module Block = struct
  type t = Typs.Block.t structured
end

module Location = struct
  type t = Typs.Location.t structured

  let unknown_get = Bindings.Location.unknown_get
end

module Module = struct
  type t = Typs.Module.t structured

  let empty = Bindings.Module.empty
  let body = Bindings.Module.body
end

module Type = struct
  type t = Typs.Type.t structured

  let parse ctx s =
    let s = Bindings.StringRef.of_string s in
    Bindings.Type.parse ctx s
end

let register_all_dialects = Bindings.register_all_dialects
