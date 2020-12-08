open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  module PassManager = struct
    (* Create a new top-level PassManager. *)
    let create =
      foreign "mlirPassManagerCreate" (Typs.Context.t @-> returning Typs.PassManager.t)


    (* Destroy the provided PassManager. *)
    let destroy = foreign "mlirPassManagerDestroy" (Typs.PassManager.t @-> returning void)

    (* Checks if a PassManager is null. *)
    let is_null = foreign "mlirPassManagerIsNull" (Typs.PassManager.t @-> returning bool)

    (* Cast a top-level PassManager to a generic OpPassManager. *)
    let to_op_pass_manager =
      foreign
        "mlirPassManagerGetAsOpPassManager"
        (Typs.PassManager.t @-> returning Typs.OpPassManager.t)


    (* Run the provided `passManager` on the given `module`. *)
    let run =
      foreign
        "mlirPassManagerRun"
        (Typs.PassManager.t @-> Typs.Module.t @-> returning Typs.LogicalResult.t)


    (* Nest an OpPassManager under the top-level PassManager, the nested
     * passmanager will only run on operations matching the provided name.
     * The returned OpPassManager will be destroyed when the parent is destroyed.
     * To further nest more OpPassManager under the newly returned one, see
     * `mlirOpPassManagerNest` below. *)
    let nested_under =
      foreign
        "mlirPassManagerGetNestedUnder"
        (Typs.PassManager.t @-> Typs.StringRef.t @-> returning Typs.OpPassManager.t)


    (* Add a pass and transfer ownership to the provided top-level mlirPassManager.
     * If the pass is not a generic operation pass or a ModulePass, a new
     * OpPassManager is implicitly nested under the provided PassManager. *)
    let add_owned_pass =
      foreign
        "mlirPassManagerAddOwnedPass"
        (Typs.PassManager.t @-> Typs.Pass.t @-> returning void)
  end

  module OpPassManager = struct
    (* Nest an OpPassManager under the provided OpPassManager, the nested
     * passmanager will only run on operations matching the provided name.
     * The returned OpPassManager will be destroyed when the parent is destroyed. *)
    let nested_under =
      foreign
        "mlirOpPassManagerGetNestedUnder"
        (Typs.OpPassManager.t @-> Typs.StringRef.t @-> returning Typs.OpPassManager.t)


    (* Add a pass and transfer ownership to the provided mlirOpPassManager. If the
     * pass is not a generic operation pass or matching the type of the provided
     * PassManager, a new OpPassManager is implicitly nested under the provided
     * PassManager. *)
    let add_owned_pass =
      foreign
        "mlirOpPassManagerAddOwnedPass"
        (Typs.OpPassManager.t @-> Typs.Pass.t @-> returning void)


    (* Print a textual MLIR pass pipeline by sending chunks of the string
   * representation and forwarding `userData to `callback`. Note that the callback
   * may be called several times with consecutive chunks of the string. *)
    let print_pass_pipeline =
      foreign
        "mlirPrintPassPipeline"
        (Typs.OpPassManager.t @-> Typs.string_callback @-> ptr void @-> returning void)


    (* Parse a textual MLIR pass pipeline and add it to the provided OpPassManager. *)
    let parse_pass_pipeline =
      foreign
        "mlirParsePassPipeline"
        (Typs.OpPassManager.t @-> Typs.StringRef.t @-> returning Typs.LogicalResult.t)
  end
end
