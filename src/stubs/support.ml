open Ctypes

(* Support API *)
module Bindings (F : FOREIGN) = struct
  open F

  (* StringRef API *)
  module StringRef = struct
    let create =
      foreign "mlirStringRefCreate" (string @-> size_t @-> returning Typs.StringRef.t)


    let of_string =
      foreign "mlirStringRefCreateFromCString" (string @-> returning Typs.StringRef.t)


    let to_string d =
      let i = getf d Typs.StringRef.length |> Unsigned.Size_t.to_int in
      let s = getf d Typs.StringRef.data in
      String.sub s 0 i
  end

  (* LogicalResult API *)
  module LogicalResult = struct
    let is_success =
      foreign "mlirLogicalResultIsSuccess" (Typs.LogicalResult.t @-> returning bool)


    let is_faiure =
      foreign "mlirLogicalResultIsFailure" (Typs.LogicalResult.t @-> returning bool)


    let success =
      foreign "mlirLogicalResultSuccess" (void @-> returning Typs.LogicalResult.t)


    let failure =
      foreign "mlirLogicalResultFailure" (void @-> returning Typs.LogicalResult.t)
  end
end
