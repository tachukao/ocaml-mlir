open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  (* Registers all dialects known to core MLIR with the provided Context.
   * This is needed before creating IR for these Dialects.
   *)
  let register_all_dialects =
    foreign "mlirRegisterAllDialects" (Typs.IR.Context.t @-> returning void)
end
