module Typs = Stubs.Typs
module Bindings = Stubs.Bindings (Mlir_generated)

module Support = struct
  module StringRef = struct
    type t = Typs.Support.StringRef.t
  end
end

module IR = struct
  module Context = struct
    type t = Typs.IR.Context.t
    let create () = Bindings.IR.Context.create  ()
  end
end
