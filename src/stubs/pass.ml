open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  module PassManager = struct
    (* Create a new top-level PassManager. *)
    let create =
      foreign
        "mlirPassManagerCreate"
        (Typs.IR.Context.t @-> returning Typs.Pass.PassManager.t)


    (* Destroy the provided PassManager. *)
    let destroy =
      foreign "mlirPassManagerDestroy" (Typs.Pass.PassManager.t @-> returning void)


    (* Checks if a PassManager is null. *)
    let is_null =
      foreign "mlirPassManagerIsNull" (Typs.Pass.PassManager.t @-> returning bool)


    (* Cast a top-level PassManager to a generic OpPassManager. *)
    let to_op_pass_manager =
      Typs.Pass.PassManager.t @-> returning Typs.Pass.OpPassManager.t


    (* Run the provided `passManager` on the given `module`. *)
    let run =
      foreign
        "mlirPassManagerRun"
        (Typs.Pass.PassManager.t
        @-> Typs.IR.Module.t
        @-> returning Typs.Support.LogicalResult.t)


    (* Nest an OpPassManager under the top-level PassManager, the nested
     * passmanager will only run on operations matching the provided name.
     * The returned OpPassManager will be destroyed when the parent is destroyed.
     * To further nest more OpPassManager under the newly returned one, see
     * `mlirOpPassManagerNest` below. *)
    let nested_under =
      foreign
        "mlirPassManagerGetNestedUnder"
        (Typs.Pass.PassManager.t
        @-> Typs.Support.StringRef.t
        @-> returning Typs.Pass.OpPassManager.t)


    (* Add a pass and transfer ownership to the provided top-level mlirPassManager.
     * If the pass is not a generic operation pass or a ModulePass, a new
     * OpPassManager is implicitly nested under the provided PassManager. *)
    let add_owned_pass =
      foreign
        "mlirPassManagerAddOwnedPass"
        (Typs.Pass.PassManager.t @-> Typs.Pass.Pass.t @-> returning void)
  end

  module OpPassManager = struct
    (* Nest an OpPassManager under the provided OpPassManager, the nested
     * passmanager will only run on operations matching the provided name.
     * The returned OpPassManager will be destroyed when the parent is destroyed. *)
    let nested_under =
      foreign
        "mlirOpPassManagerGetNestedUnder"
        (Typs.Pass.OpPassManager.t
        @-> Typs.Support.StringRef.t
        @-> returning Typs.Pass.OpPassManager.t)


    (* Add a pass and transfer ownership to the provided mlirOpPassManager. If the
     * pass is not a generic operation pass or matching the type of the provided
     * PassManager, a new OpPassManager is implicitly nested under the provided
     * PassManager. *)
    let add_owned_pass =
      foreign
        "mlirOpPassManagerAddOwnedPass"
        (Typs.Pass.OpPassManager.t @-> Typs.Pass.Pass.t @-> returning void)


    (* Print a textual MLIR pass pipeline by sending chunks of the string
   * representation and forwarding `userData to `callback`. Note that the callback
   * may be called several times with consecutive chunks of the string. *)
    let print_pass_pipeline =
      foreign
        "mlirPrintPassPipeline"
        (Typs.Pass.OpPassManager.t
        @-> Typs.string_callback
        @-> ptr void
        @-> returning void)


    (* Parse a textual MLIR pass pipeline and add it to the provided OpPassManager. *)
    let parse_pass_pipeline =
      foreign
        "mlirParsePassPipeline"
        (Typs.Pass.OpPassManager.t
        @-> Typs.Support.StringRef.t
        @-> returning Typs.Support.LogicalResult.t)
  end
end
