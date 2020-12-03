open Ctypes
module T = Types.Bindings (Types_bindings)

let stringref_callback =
  Foreign.funptr Ctypes.(T.Support.StringRef.t @-> ptr void @-> returning void)


module Bindings (F : FOREIGN) = struct
  open F

  module Support = struct
    module StringRef = struct
      open T.Support.StringRef

      type t = T.Support.StringRef.t

      let callback = stringref_callback
      let create = foreign "mlirStringRefCreate" (string @-> size_t @-> returning t)
      let of_string = foreign "mlirStringRefCreateFromCString" (string @-> returning t)
    end

    module LogicalResult = struct
      open T.Support.LogicalResult

      type t = T.Support.LogicalResult.t

      let is_success = foreign "mlirLogicalResultIsSuccess" (t @-> returning bool)
      let is_faiure = foreign "mlirLogicalResultIsFailure" (t @-> returning bool)
      let success = foreign "mlirLogicalResultSuccess" (void @-> returning t)
      let failure = foreign "mlirLogicalResultFailure" (void @-> returning t)
    end
  end

  module IR = struct
    module Context = struct
      open T.IR.Context

      type t = T.IR.Context.t

      let create = foreign "mlirContextCreate" (void @-> returning t)
      let equal = foreign "mlirContextEqual" (t @-> t @-> returning bool)
      let is_null = foreign "mlirContextIsNull" (t @-> returning bool)
      let destroy = foreign "mlirContextDestroy" (t @-> returning void)

      let set_allow_unregistered_dialects =
        foreign "mlirContextSetAllowUnregisteredDialects" (t @-> bool @-> returning void)


      let get_allow_unregistered_dialects =
        foreign "mlirContextGetAllowUnregisteredDialects" (t @-> returning bool)


      let num_registered_dialects =
        foreign "mlirContextGetNumRegisteredDialects" (t @-> returning long)


      let num_loaded_dialects =
        foreign "mlirContextGetNumLoadedDialects" (t @-> returning long)


      let get_or_load_dialect =
        foreign
          "mlirContextGetOrLoadDialect"
          (t @-> T.Support.StringRef.t @-> returning T.IR.Dialect.t)
    end

    module Dialect = struct
      open T.IR.Dialect

      type t = T.IR.Dialect.t

      let context = foreign "mlirDialectGetContext" (t @-> returning T.IR.Context.t)
      let is_null = foreign "mlirDialectIsNull" (t @-> returning bool)
      let equal = foreign "mlirDialectEqual" (t @-> t @-> returning bool)

      let namespace =
        foreign "mlirDialectGetNamespace" (t @-> returning T.Support.StringRef.t)
    end

    module Location = struct
      open T.IR.Location

      type t = T.IR.Location.t

      let file_line_col_get =
        foreign
          "mlirLocationFileLineColGet"
          (T.IR.Context.t @-> T.Support.StringRef.t @-> uint @-> uint @-> returning t)


      let unknown_get = foreign "mlirLocationUnknownGet" (T.IR.Context.t @-> returning t)
      let context = foreign "mlirLocationGetContext" (t @-> returning T.IR.Context.t)
      let is_null = foreign "mlirLocationIsNull" (t @-> returning bool)
      let equal = foreign "mlirLocationEqual" (t @-> t @-> returning bool)

      let print =
        foreign
          "mlirLocationPrint"
          (t @-> Support.StringRef.callback @-> ptr void @-> returning void)
    end

    module Module = struct
      let empty =
        foreign "mlirModuleCreateEmpty" (T.IR.Location.t @-> returning T.IR.Module.t)


      let parse =
        foreign
          "mlirModuleCreateParse"
          (T.IR.Context.t @-> T.Support.StringRef.t @-> returning T.IR.Module.t)


      let context =
        foreign "mlirModuleGetContext" (T.IR.Module.t @-> returning T.IR.Context.t)


      let body = foreign "mlirModuleGetBody" (T.IR.Module.t @-> returning T.IR.Block.t)
      let is_null = foreign "mlirModuleIsNull" (T.IR.Module.t @-> returning bool)
      let destroy = foreign "mlirModuleDestroy" (T.IR.Module.t @-> returning void)

      let operation =
        foreign "mlirModuleGetOperation" (T.IR.Module.t @-> returning T.IR.Operation.t)
    end

    module OperationState = struct
      open T.IR.OperationState

      let get =
        foreign
          "mlirOperationStateGet"
          (T.Support.StringRef.t @-> T.IR.Location.t @-> returning t)


      let add_results =
        foreign
          "mlirOperationStateAddResults"
          (ptr t @-> intptr_t @-> ptr T.IR.Type.t @-> returning void)


      let add_operands =
        foreign
          "mlirOperationStateAddOperands"
          (ptr t @-> intptr_t @-> ptr T.IR.Value.t @-> returning void)


      let add_regions =
        foreign
          "mlirOperationStateAddOwnedRegions"
          (ptr t @-> intptr_t @-> ptr T.IR.Region.t @-> returning void)


      let add_successors =
        foreign
          "mlirOperationStateAddSuccessors"
          (ptr t @-> intptr_t @-> ptr T.IR.Block.t @-> returning void)


      let add_attributes =
        foreign
          "mlirOperationStateAddAttributes"
          (ptr t @-> intptr_t @-> ptr T.IR.NamedAttribute.t @-> returning void)
    end

    module OpPrintingFlags = struct
      open T.IR.OpPrintingFlags

      let create =
        foreign "mlirOpPrintingFlagsCreate" (void @-> returning T.IR.OpPrintingFlags.t)


      let destroy = foreign "mlirOpPrintingFlagsDestroy" (t @-> returning void)

      let large_element_limit =
        foreign
          "mlirOpPrintingFlagsElideLargeElementsAttrs"
          (t @-> intptr_t @-> returning void)


      let enable_debug_info =
        foreign "mlirOpPrintingFlagsEnableDebugInfo" (t @-> bool @-> returning void)


      let print_generic_op_form =
        foreign "mlirOpPrintingFlagsPrintGenericOpForm" (t @-> returning void)


      let use_local_scope =
        foreign "mlirOpPrintingFlagsUseLocalScope" (t @-> returning void)
    end
  end
end
