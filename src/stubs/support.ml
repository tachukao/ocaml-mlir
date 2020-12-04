open Ctypes

(* Support API *)
module Bindings (F : FOREIGN) = struct
  open F

  (* StringRef API *)
  module StringRef = struct
    let create =
      foreign
        "mlirStringRefCreate"
        (string @-> size_t @-> returning Typs.StringRef.t)


    let of_string =
      foreign
        "mlirStringRefCreateFromCString"
        (string @-> returning Typs.StringRef.t)
  end

  (* LogicalResult API *)
  module LogicalResult = struct
    let is_success =
      foreign
        "mlirLogicalResultIsSuccess"
        (Typs.LogicalResult.t @-> returning bool)


    let is_faiure =
      foreign
        "mlirLogicalResultIsFailure"
        (Typs.LogicalResult.t @-> returning bool)


    let success =
      foreign "mlirLogicalResultSuccess" (void @-> returning Typs.LogicalResult.t)


    let failure =
      foreign "mlirLogicalResultFailure" (void @-> returning Typs.LogicalResult.t)
  end
end
