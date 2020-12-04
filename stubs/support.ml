open Ctypes

(* Support API *)
module Bindings (F : FOREIGN) = struct
  open F

  (* StringRef API *)
  module StringRef = struct
    let create =
      foreign
        "mlirStringRefCreate"
        (string @-> size_t @-> returning Typs.Support.StringRef.t)


    let of_string =
      foreign
        "mlirStringRefCreateFromCString"
        (string @-> returning Typs.Support.StringRef.t)
  end

  (* LogicalResult API *)
  module LogicalResult = struct
    let is_success =
      foreign
        "mlirLogicalResultIsSuccess"
        (Typs.Support.LogicalResult.t @-> returning bool)


    let is_faiure =
      foreign
        "mlirLogicalResultIsFailure"
        (Typs.Support.LogicalResult.t @-> returning bool)


    let success =
      foreign "mlirLogicalResultSuccess" (void @-> returning Typs.Support.LogicalResult.t)


    let failure =
      foreign "mlirLogicalResultFailure" (void @-> returning Typs.Support.LogicalResult.t)
  end
end
