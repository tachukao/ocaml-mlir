module Typs = Stubs.Typs
module Bindings = Stubs.Bindings (Mlir_generated)

module StringRef = struct
  type t = Typs.StringRef.t
end

module Context = struct
  type t = Typs.Context.t

  let create () = Bindings.Context.create ()
end
